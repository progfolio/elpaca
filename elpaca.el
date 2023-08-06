;;; elpaca.el --- An elisp package manager           -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Nicholas Vollmer

;; Author: Nicholas Vollmer
;; URL: https://github.com/progfolio/elpaca
;; Created: Jan 1, 2022
;; Keywords: tools, convenience, lisp
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.0.0

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Would-be Developers:
;; This package has a few idiosyncracies I'm experimenting with.
;; I've defined all structs to end with the character "<", such that slots are
;; accessed via a faux "arrow" like syntax. e.g. elpaca<-repo-dir
;; The main data structures of this program are "elpacas" and "queues".
;; They are abbreviated as "e" and "q" when used as function parameters.

;;; Code:
(eval-and-compile (require 'cl-lib))
(eval-when-compile (require 'subr-x))
(cl-declaim (optimize (safety 0) (speed 3)))
(require 'elpaca-process)
(declare-function autoload-rubric "autoload")
(declare-function info-initialize "info")
(declare-function url-filename    "url-parse")
(defvar autoload-timestamps)
(defvar generated-autoload-file)
(defvar Info-directory-list)
(defconst elpaca--inactive-states '(blocked finished failed))
(defvar elpaca-installer-version -1)
(unless (or noninteractive (= elpaca-installer-version 0.5)) (warn "Elpaca installer version mismatch"))
(unless (executable-find "git") (error "Elpaca unable to find git executable"))
(when (and (not after-init-time) load-file-name (featurep 'package))
  (warn "Package.el loaded before Elpaca"))

(defgroup elpaca nil "An elisp package manager." :group 'applications :prefix "elpaca-")

(defface elpaca-finished '((t (:inherit success))) "Indicates an order is finished.")
(defface elpaca-busy '((t (:inherit warning :inverse-video t)))
  "Indicates order's subprocess has not produced output in `elpaca-busy-interval'.")
(defface elpaca-blocked '((t (:inherit warning))) "Indicates an order is blocked.")
(defface elpaca-failed '((t (:inherit error))) "Indicates an order has failed.")

(defcustom elpaca-status-faces '((blocked  . elpaca-blocked)
                                 (finished . elpaca-finished)
                                 (failed   . elpaca-failed)
                                 (busy     . elpaca-busy))
  "Alist mapping order statuses to faces."
  :type '(alist :key-type symbol :options (blocked finished failed busy) :value-type face))

(defvar elpaca--pre-built-steps
  '(elpaca--queue-dependencies elpaca--add-info-path elpaca--activate-package)
  "List of steps for packages which are already built.")

(defvar elpaca-after-init-time nil "Time after `elpaca-after-init-hook' is run.")
(defcustom elpaca-after-init-hook nil
  "Elpaca's analogue to `after-init-hook'.
This is run after all orders queued during init have finished processing.
It is only run once after init.
Note a blocked process will prevent this hook from being run."
  :type 'hook)

(defcustom elpaca-post-queue-hook nil
  "Hook run after a queue is finished processing.
Note blocked or failed orders will prevent this hook from being run."
  :type 'hook)

(defcustom elpaca-queue-limit nil
  "When non-nil, limit the number of orders which can be processed at once."
  :type 'integer)

(defcustom elpaca-cache-autoloads t
  "If non-nil, cache package autoloads and load all at once.
Results in faster start-up time." :type 'boolean)

(defcustom elpaca-directory (expand-file-name "elpaca" user-emacs-directory)
  "Location of the elpaca package store." :type 'directory)

(defvar elpaca-cache-directory (expand-file-name "cache" elpaca-directory)
  "Location of the cache directory.")

(defvar elpaca-builds-directory (expand-file-name "builds" elpaca-directory)
  "Location of the builds directory.")

(defvar elpaca-repos-directory (expand-file-name "repos" elpaca-directory)
  "Location of the repos directory.")

(defcustom elpaca-makeinfo-executable (executable-find "makeinfo")
  "Path of the makeinfo executable." :type '(file :must-match t))

(defcustom elpaca-install-info-executable (executable-find "install-info")
  "Path of the install-info executable." :type '(file :must-match t))

(defvar elpaca--log-timer nil "Timer to debounce order info printing.")
(defcustom elpaca-log-interval 0.02
  "Number of idle seconds to wait before updating log buffer.
Setting this to too low may cause the status buffer to block more.
Setting it too high causes prints fewer status updates."
  :type 'number)

(defcustom elpaca-busy-interval 60
  "Seconds to wait between subprocess outputs before declaring process blocked."
  :type 'number)

(defcustom elpaca-build-steps '(elpaca--clone
                                elpaca--configure-remotes
                                elpaca--checkout-ref
                                elpaca--run-pre-build-commands
                                elpaca--clone-dependencies
                                elpaca--link-build-files
                                elpaca--generate-autoloads-async
                                elpaca--byte-compile
                                elpaca--compile-info
                                elpaca--install-info
                                elpaca--add-info-path
                                elpaca--run-post-build-commands
                                elpaca--activate-package)
  "List of steps which are run when installing/building a package."
  :type '(repeat function))

(defvar elpaca--debug-init init-file-debug "Preserves --debug-init option.")

(defvar elpaca-default-files-directive
  '("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
              "README*" "*-pkg.el"))
  "Default value for the `:files' directive in recipes.
It is also spliced in at any point where the `:defaults' keyword
is used in a `:files' directive.")

(defvar elpaca-order-defaults (list :protocol 'https :inherit t :depth 1)
  "Default order modifications.")

(defun elpaca-order-defaults (_order) "Matches any order." elpaca-order-defaults)

(defcustom elpaca-order-functions '(elpaca-order-defaults)
  "Abnormal hook run to alter orders.
Each element must be a unary function which accepts an order.
An order may be nil, a symbol naming a package, or a plist.
The function may return nil or a plist to be merged with the order.
This hook is run via `run-hook-with-args-until-success'."
  :type 'hook)

(defcustom elpaca-recipe-functions nil
  "Abnormal hook run to alter recipes.
Each element must be a unary function which accepts an recipe plist.
The function may return nil or a plist to be merged with the recipe.
This hook is run via `run-hook-with-args-until-success'."
  :type 'hook)

(defvar elpaca-menu-extensions--cache nil "Cache for `elpaca-menu-extenions' items.")
(defun elpaca-menu-extensions (request)
  "Return menu item REQUEST."
  (or (and (eq request 'index) elpaca-menu-extensions--cache)
      (setq elpaca-menu-extensions--cache
            (list (cons 'elpaca-use-package
                        (list :source "Elpaca extensions"
                              :description "Elpaca use-package support."
                              :recipe (list :package "elpaca-use-package"
                                            :repo "https://github.com/progfolio/elpaca.git"
                                            :files '("extensions/elpaca-use-package.el")
                                            :main "extensions/elpaca-use-package.el"
                                            :build '(:not elpaca--compile-info))))))))

(defcustom elpaca-menu-functions
  '( elpaca-menu-extensions elpaca-menu-org elpaca-menu-melpa elpaca-menu-non-gnu-devel-elpa
     elpaca-menu-gnu-devel-elpa elpaca-menu-non-gnu-elpa elpaca-menu-gnu-elpa )
  "Abnormal hook to lookup packages in menus.
Each function is passed a request, which may be any of the following symbols:
  - `index`
     Must return a alist of the menu's package candidates.
     Each candidate is a cell of form:
     (PACKAGE-NAME . (:source SOURCE-NAME :recipe RECIPE-PLIST))
  - `update`
     Updates the menu's package candidate list."
  :type 'hook)

;;@FIX: emacsclient doesn't show build when server needs to be started.
(defcustom elpaca-hide-initial-build nil
  "When non-nil, hide `elpaca-log' during init time builds." :type 'boolean)
(defvar elpaca--ibc initial-buffer-choice "User's `initial-buffer-choice'.")
(defun elpaca--set-ibc (_ new &rest _args)
  "Update `elpaca--ibc' when `initial-buffer-choice' set to NEW."
  (unless (eq new #'elpaca--ibs) (setq elpaca--ibc new)))
(add-variable-watcher 'initial-buffer-choice #'elpaca--set-ibc)
(defvar elpaca--ibs-set nil)

(defcustom elpaca-verbosity 0 "Maximum event verbosity level shown in logs."
  :type 'integer)
(defcustom elpaca-default-remote-name "origin" "Default remote name." :type 'string)

(defcustom elpaca-ignored-dependencies
  `( emacs cl-lib cl-generic nadvice org org-mode map seq json project auth-source-pass
     let-alist flymake jsonrpc eldoc erc ntlm python so-long soap-client
     svg ruby-mode verilog-mode xref
     ,@(unless (< emacs-major-version 28) '(transient))
     ,@(unless (< emacs-major-version 29) '(external-completion use-package bind-key eglot)))
  "List of items which are not installed unless the user explicitly requests them."
  :type '(repeat symbol))

(defvar elpaca-overriding-prompt nil "Overriding prompt for interactive functions.")

(defun elpaca--read-file (path)
  "Read file at PATH into memory."
  (when (file-exists-p path)
    (condition-case-unless-debug err
        (with-temp-buffer (insert-file-contents path) (read (current-buffer)))
      ((error) (warn "Error reading %S into memory: %S" path err) nil))))

(defmacro elpaca--write-file (file &rest body)
  "Write FILE using BODY.
`standard-output' and print variables are lexically bound for convenience.
e.g. elisp forms may be printed via `prin1'."
  (declare (indent 1) (debug t))
  `(let ((coding-system-for-write 'utf-8))
     (with-temp-file ,file
       (let* ((standard-output (current-buffer))
              print-circle print-level print-length)
         ,@body
         nil))))

(defvar elpaca--menu-items-cache
  (elpaca--read-file (expand-file-name "menu-items.eld" elpaca-cache-directory))
  "Cache for menu candidates.")

(cl-defstruct (elpaca-q< (:constructor elpaca-q<-create)
                         (:type list)
                         (:copier nil)
                         (:named))
  "Queue to hold elpacas."
  (type (when (or (not after-init-time)
                  (let ((init (expand-file-name "init.el" user-emacs-directory)))
                    (member init (list load-file-name (buffer-file-name)))))
          'init))
  (id  (if (boundp 'elpaca--queues) (length elpaca--queues) 0))
  (processed 0)
  (status 'incomplete)
  (time (current-time))
  autoloads forms elpacas)

(defvar elpaca--queues (list (elpaca-q<-create)) "List of elpaca queue objects.")

(defun elpaca-merge-plists (&rest plists)
  "Return plist with set of unique keys from PLISTS.
Values for each key are that of the right-most plist containing that key."
  (let ((plists (delq nil plists))
        current plist)
    (while (setq current (pop plists))
      (while current (setq plist (plist-put plist (pop current) (pop current)))))
    plist))

(defun elpaca--write-menu-cache ()
  "Write menu item cache to disk."
  (unless (file-exists-p elpaca-cache-directory)
    (make-directory elpaca-cache-directory))
  (elpaca--write-file (expand-file-name "menu-items.eld" elpaca-cache-directory)
    (prin1 elpaca--menu-items-cache)))

(defun elpaca--menu-items (&optional cache menus)
  "Return alist of `elpaca-menu-functions' candidates from MENUS.
CACHE may be any of the following symbols:
  `t` Return cache or recompute if nil. Ignore MENUS.
  `nil` Recompute items, ignoring cache altogether.
  `recache` Invalidate and recompute cache considering MENUS.
See `elpaca-menu-functions' for valid values of MENUS."
  (or (and (eq cache t) elpaca--menu-items-cache)
      (let ((items (sort (copy-tree (cl-loop for fn in (or menus elpaca-menu-functions)
                                             append (and (functionp fn) (funcall fn 'index))))
                         (lambda (a b) (string-lessp (car a) (car b))))))
        (when cache
          (setq elpaca--menu-items-cache items)
          (elpaca--write-menu-cache))
        items)))

(defsubst elpaca-alist-get (key alist &optional default)
  "Return KEY's value in ALIST or DEFAULT.
Simplified, faster version of `alist-get'."
  (or (cdr (assq key alist)) default))

(defsubst elpaca--first (obj)
  "Return `car' of OBJ if it is a list, else OBJ."
  (if (listp obj) (car obj) obj))

(defsubst elpaca--q (e)
  "Return E's Q."
  (and e (car (last elpaca--queues (1+ (elpaca<-queue-id e))))))

;;;###autoload
(defun elpaca-menu-item (symbol &optional items interactive)
  "Return menu item matching SYMBOL in ITEMS or `elpaca-menu-functions' cache.
If INTERACTIVE is non-nil or SYMBOL is t, prompt for item.
Optional FILTER must be a function which accepts a candidate.
If it returns nil, the candidate is removed from the candidate list."
  (interactive (list t nil t))
  (let* ((items (or items (elpaca--menu-items t)))
         (symbol (if (or interactive (eq symbol t))
                     (read (completing-read
                            (or elpaca-overriding-prompt "Menu Item: ")
                            (mapcar (lambda (c)
                                      (let ((data (cdr c)))
                                        (format "%-30s %s %s" (car c)
                                                (or (plist-get data :description) "")
                                                (or (plist-get data :source) ""))))
                                    items)))
                   symbol))
         (item (assoc symbol items)))
    (when interactive
      (message "menu-item copied to kill-ring:\n%S" item)
      (kill-new (format "%S" item)))
    item))

;;;###autoload
(defun elpaca-update-menus (&rest menus)
  "Update all menus in MENUS or `elpaca-menu-functions'.
When called interactively with \\[universal-argument] update all menus."
  (interactive (if (equal current-prefix-arg '(4))
                   elpaca-menu-functions
                 (mapcar #'intern (completing-read-multiple "Update Menus: "
                                                            elpaca-menu-functions))))
  (let ((elpaca-menu-functions (or menus elpaca-menu-functions)))
    (run-hook-with-args 'elpaca-menu-functions 'update))
  (elpaca--menu-items 'recache))

(defsubst elpaca--inheritance-disabled-p (obj)
  "Return t if OBJ explicitly has :inherit nil key val, nil otherwise."
  (when-let (((listp obj))
             (member (plist-member obj :inherit)))
    (not (cadr member))))

;;;###autoload
(defun elpaca-recipe (order &optional items interactive)
  "Return recipe computed from ORDER.
ORDER is any of the following values:
  - nil. The order is prompted for.
  - an item symbol, looked up in ITEMS or `elpaca-menu-functions' cache.
  - an order list of the form: \\='(ITEM . PROPS).
When INTERACTIVE is non-nil, `yank' the recipe to the clipboard."
  (interactive (list (if-let ((elpaca-overriding-prompt "Recipe: ")
                              (item (elpaca-menu-item t (append (elpaca--custom-candidates t)
                                                                (elpaca--menu-items t)))))
                         (cons (car item) (plist-get (cdr item) :recipe))
                       (user-error "No recipe selected"))
                     nil t))
  (let* ((props (cdr-safe order))
         (id (elpaca--first order))
         (nonheritablep (elpaca--inheritance-disabled-p props))
         (mods (unless nonheritablep (run-hook-with-args-until-success
                                      'elpaca-order-functions order)))
         (item (unless (or interactive ;; we already queried for this.
                           (elpaca--inheritance-disabled-p
                            (elpaca-merge-plists
                             mods (plist-member props :inherit))))
                 (plist-get (cdr (elpaca-menu-item (or id t) items)) :recipe)))
         (r (elpaca-merge-plists item mods props)))
    (unless (plist-get r :package) (setq r (plist-put r :package (symbol-name id))))
    (when-let ((recipe-mods (run-hook-with-args-until-success 'elpaca-recipe-functions r)))
      (setq r (elpaca-merge-plists r recipe-mods)))
    (when interactive
      (setq r (elpaca-merge-plists (append (list :package (symbol-name (car-safe id)))
                                           (cdr-safe id))
                                   r))
      (kill-new (format "%S" r))
      (message "%S recipe copied to kill-ring:\n%S" (plist-get r :package) r))
    r))

(defsubst elpaca--emacs-path ()
  "Return path to running Emacs."
  (concat invocation-directory invocation-name))

(defsubst elpaca--repo-name (string)
  "Return repo name portion of STRING."
  (setq string (directory-file-name string)) ;; remove external :repo trailing slash
  (file-name-base (substring string (- (string-match-p "/" (reverse string))))))

(defsubst elpaca--repo-user (string)
  "Return user name portion of STRING."
  (substring string 0 (string-match-p "/" string)))

(defun elpaca--repo-type (string)
  "Return type of :repo STRING.
Type is `local' for a local filesystem path, `remote' for a remote URL, or nil."
  (cond ((string-match-p "^[/~]" string) 'local)
        ((string-match-p ":" string) 'remote)))

(defvar elpaca--repo-dirs nil "List of registered repository directories.")
(defun elpaca-repo-dir (recipe)
  "Return path to repo given RECIPE."
  (let* ((local-repo (plist-get recipe :local-repo))
         (url (plist-get recipe :url))
         (repo (plist-get recipe :repo))
         (pkg (plist-get recipe :package))
         (host (or (plist-get recipe :host) (plist-get recipe :fetcher)))
         (hostname (and host (prin1-to-string host 'noescape)))
         (user nil)
         (info (concat url repo hostname))
         (key (or (and info (> (length info) 0) (intern info))
                  (error "Cannot determine URL from recipe: %S" recipe)))
         (mono-repo (alist-get key elpaca--repo-dirs))
         (dirs (and (not mono-repo) (mapcar #'cdr elpaca--repo-dirs)))
         (name (cond
                (local-repo
                 (if-let ((owner (assoc local-repo dirs)))
                     (error ":local-repo %S owned by %s" local-repo (cdr owner))
                   local-repo))
                (mono-repo (car mono-repo))
                (url
                 (unless (featurep 'url-parse) (require 'url-parse))
                 (file-name-base (directory-file-name (url-filename
                                                       (url-generic-parse-url url)))))
                (repo
                 (if (eq (elpaca--repo-type repo) 'local)
                     (file-name-base (directory-file-name repo))
                   (when host (setq user (elpaca--repo-user repo)))
                   (elpaca--repo-name repo)))
                (pkg pkg)
                (t (error "Unable to determine repo name"))))
         (dir (if (assoc name dirs)
                  (string-join (list name hostname user) ".")
                (and name (replace-regexp-in-string "\\.el$" "" name)))))
    (unless mono-repo (push (cons key (cons dir pkg)) elpaca--repo-dirs))
    (file-name-as-directory (expand-file-name dir elpaca-repos-directory))))

(defun elpaca-build-dir (recipe)
  "Return RECIPE's build dir."
  (expand-file-name (plist-get recipe :package) elpaca-builds-directory))

(defun elpaca--repo-uri (recipe)
  "Return repo URI from RECIPE."
  (cl-destructuring-bind (&key (protocol 'https)
                               url
                               fetcher
                               (host fetcher)
                               (repo url) &allow-other-keys)
      recipe
    (pcase (elpaca--repo-type (or repo (error "Unable to determine recipe URL")))
      ('remote repo)
      ('local  (expand-file-name repo))
      (_ (let ((p (pcase protocol
                    ('https '("https://" . "/"))
                    ('ssh   '("git@" . ":"))
                    (_      (signal 'wrong-type-argument `((https ssh) ,protocol)))))
               (h (pcase host
                    ('github       "github.com")
                    ('gitlab       "gitlab.com")
                    ('codeberg     "codeberg.org")
                    ('sourcehut    "git.sr.ht")
                    ((pred stringp) host)
                    (_ (signal 'wrong-type-argument
                               `(:host (github gitlab codeberg sourcehut stringp)
                                       ,host ,recipe))))))
           (concat (car p) h (cdr p) (when (eq host 'sourcehut) "~") repo
                   (unless (eq host 'sourcehut) ".git")))))))

(cl-defstruct (elpaca< (:constructor elpaca<--create) (:type list) (:named))
  "Order for queued processing."
  id package order statuses
  repo-dir build-dir mono-repo
  files build-steps recipe siblings
  dependencies dependents -dependencies
  (queue-id (1- (length elpaca--queues)))
  (queue-time (current-time))
  (init (not after-init-time))
  process log builtp)

(defun elpaca--queued (&optional n)
  "Return list of elpacas from Nth queue.
If N is nil return a list of all queued elpacas."
  (if n (elpaca-q<-elpacas (nth n elpaca--queues))
    (cl-loop for queue in elpaca--queues append (elpaca-q<-elpacas queue))))

(defsubst elpaca--mono-repo (id repo-dir)
  "Return previously queued E with REPO-DIR other than ID."
  (cl-some (lambda (queued)
             (and-let* ((e (cdr queued))
                        ((not (eq (elpaca<-id e) id)))
                        ((equal repo-dir (elpaca<-repo-dir e)))
                        e)))
           (reverse (elpaca--queued))))

(defun elpaca--build-steps (recipe &optional builtp clonedp mono-repo)
  "Return list of build functions for RECIPE.
BUILTP, CLONEDP, and MONO-REPO control which steps are excluded."
  (when-let ((defaults (if builtp elpaca--pre-built-steps elpaca-build-steps))
             (steps (let* ((build (plist-member recipe :build))
                           (steps (cadr build))
                           (removep (and (eq (car-safe steps) :not) (pop steps))))
                      (cond
                       ((or (not build) (eq steps t)) defaults)
                       (removep (cl-set-difference defaults steps))
                       ((listp steps) steps)))))
    (if builtp
        steps
      (when mono-repo
        (setq steps
              (cl-set-difference steps '(elpaca--clone elpaca--configure-remotes elpaca--checkout-ref))))
      (when clonedp (setq steps (delq 'elpaca--clone steps)))
      steps)))

(declare-function elpaca-log-defaults "elpaca-log")
(defcustom elpaca-log-functions #'elpaca-log-defaults
  "Hook run prior to logging.
It's functions should return either:
  - t to log with the buffer's default filter.
  - a string which is used as the search query.
  - `silent' to prevent logging altogether.
  - nil to skip the function.

The first function, if any, which returns a non-nil is used." :type 'hook)

(defvar elpaca--log-request-time nil "Time of most recent log event.")
(declare-function elpaca-log "elpaca-log")
(defun elpaca--maybe-log ()
  "Log if `elpaca-log-functions' return non-nil."
  (when-let ((query (run-hook-with-args-until-success 'elpaca-log-functions)))
    (require 'elpaca-log)
    (unless (eq query 'silent)
      (setq elpaca--log-request-time (current-time)) ;;@MAYBE: move into elpaca-log?
      (elpaca-log (cond ((eq query t) nil)
                        ((stringp query) query)
                        (t (signal 'wrong-type-error `((stringp t) ,query))))))))

(defvar elpaca-log-buffer)
(defun elpaca--ibs ()
  "Return initial status buffer if `elpaca-hide-initial-build' is nil."
  (when (elpaca--maybe-log)
    (with-current-buffer elpaca-log-buffer
      (when (equal initial-buffer-choice #'elpaca--ibs) (setq initial-buffer-choice elpaca--ibc))
      (setq-local elpaca-ui-want-tail nil)
      (current-buffer))))

(defun elpaca<-create (order)
  "Create a new elpaca struct from ORDER."
  (let* ((status 'queued)
         (info "Package queued")
         (id (elpaca--first order))
         (recipe (condition-case-unless-debug err (elpaca-recipe order)
                   ((error) (setq status 'struct-failed
                                  info (format "No recipe: %S" err))
                    nil)))
         (repo-dir (and recipe (condition-case-unless-debug err (elpaca-repo-dir recipe)
                                 ((error) (setq status 'struct-failed
                                                info (format "Unable to determine repo dir: %S" err))
                                  nil))))
         (build-dir (and recipe (elpaca-build-dir recipe)))
         (clonedp (and repo-dir (file-exists-p repo-dir)))
         (builtp (and clonedp build-dir (file-exists-p build-dir)))
         (mono-repo (when-let (((not builtp))
                               (e (elpaca--mono-repo id repo-dir)))
                      (when (and (eq (elpaca<-queue-id e) (elpaca-q<-id (car elpaca--queues)))
                                 (not (memq 'ref-checked-out (elpaca<-statuses e))))
                        (setq status 'blocked info (concat "Waiting on monorepo " repo-dir))
                        (cl-pushnew id (elpaca<-siblings e)))
                      e))
         (build-steps (elpaca--build-steps recipe builtp clonedp mono-repo)))
    (unless (or builtp elpaca--ibs-set elpaca-hide-initial-build elpaca-after-init-time)
      (setq initial-buffer-choice #'elpaca--ibs elpaca--ibs-set t))
    (when (memq id elpaca-ignored-dependencies)
      (setq elpaca-ignored-dependencies (delq id elpaca-ignored-dependencies)))
    (elpaca<--create
     :id id :package (symbol-name id) :order order :statuses (list status)
     :repo-dir repo-dir :build-dir build-dir :mono-repo mono-repo
     :build-steps build-steps :recipe recipe :builtp builtp
     :log (list (list status nil info)))))

(defsubst elpaca--status (e) "Return E's status." (car (elpaca<-statuses e)))

(declare-function elpaca-status "elpaca-status")
(declare-function elpaca-ui--update-search-query "elpaca-ui")
(defun elpaca--update-log-buffer ()
  "Update views in `elpaca-log-buffer'."
  (when-let ((log (bound-and-true-p elpaca-log-buffer))
             ((get-buffer-window log t))) ;; log buffer visible
    (with-current-buffer log (elpaca-ui--update-search-query log))))

(defun elpaca--log (e text &optional verbosity replace)
  "Store TEXT in E's log.
Each event is of the form: (STATUS TIME TEXT (or VERBOSITY 0))
If REPLACE is non-nil, the most recent log entry is replaced."
  (let ((event (list (elpaca--status e) (current-time) text (or verbosity 0))))
    (if replace
        (setf (car (elpaca<-log e)) event)
      (push event (elpaca<-log e)))))

(defvar elpaca--waiting nil "Non-nil when `elpaca-wait' is polling.")
(defvar elpaca--status-counts nil "Status counts for UI progress bar.")
(defun elpaca--count-statuses ()
  "Update `elpaca--status-counts'."
  (cl-loop with statuses for q in elpaca--queues
           do (cl-loop for (_ . e) in (elpaca-q<-elpacas q)
                       for status = (elpaca--status e)
                       for state = (if (memq status elpaca--inactive-states) status 'other)
                       do (cl-incf (alist-get state statuses 0)))
           finally return statuses))

(defun elpaca--signal (e &optional info status replace verbosity)
  "Signal a change to E. Return nil.
If INFO is non-nil, log and possibly print it in `elpaca-log-buffer'.
If REPLACE is non-nil, E's log is updated instead of appended.
If VERBOSITY is non-nil, log event is given that verbosity number.
If STATUS is non-nil and is not E's current STATUS, signal E's dependents to
check (and possibly change) their statuses."
  (let* ((new-status-p (and status (not (eq status (elpaca--status e)))))
         (siblings (elpaca<-siblings e))
         (queued (and (or new-status-p siblings) (elpaca--queued))))
    (when-let ((new-status-p)
               ((push status (elpaca<-statuses e)))
               ((memq status elpaca--inactive-states)))
      (setq elpaca--status-counts (elpaca--count-statuses))
      (dolist (d (elpaca<-dependents e)) (elpaca--check-status e (elpaca-alist-get d queued)))
      (when (and (not elpaca-after-init-time) (eq status 'failed))
        (elpaca--maybe-log)))
    (when-let ((siblings (elpaca<-siblings e))
               (statuses (elpaca<-statuses e))
               ((or (memq 'ref-checked-out statuses) (memq 'queueing-deps statuses)))
               (sibling t))
      (while (setq sibling (elpaca-alist-get (pop (elpaca<-siblings e)) queued))
        (push 'ref-checked-out     (elpaca<-statuses sibling))
        (push 'unblocked-mono-repo (elpaca<-statuses sibling))
        (elpaca--continue-build sibling)))
    (when info (elpaca--log e info verbosity replace))
    (when (<= (or verbosity 0) elpaca-verbosity)
      (when elpaca--log-timer (cancel-timer elpaca--log-timer))
      (if elpaca--waiting ; Don't set timer. We're already polling.
          (elpaca--update-log-buffer)
        (setq elpaca--log-timer
              (and info (run-at-time elpaca-log-interval nil #'elpaca--update-log-buffer))))))
  nil)

(defun elpaca--fail (e &optional reason)
  "Fail E for REASON."
  (unless (eq (elpaca--status e) 'failed)
    (let ((q (elpaca--q e)))
      (setf (elpaca-q<-forms q) (assq-delete-all (elpaca<-id e) (elpaca-q<-forms q))))
    (when-let ((p (elpaca<-process e)) ((process-live-p p))) (kill-process p))
    (elpaca--signal e reason 'failed)
    (elpaca--finalize e)))

(defun elpaca-get (item)
  "Return queued E associated with ITEM."
  (elpaca-alist-get item (elpaca--queued)))

(defun elpaca--run-build-commands (commands)
  "Run build COMMANDS."
  (dolist (command (if (listp (car-safe commands)) commands (list commands)))
    (message "Running command: %S" command)
    (if (cl-every #'stringp command)
        (apply #'elpaca-process-poll command)
      (eval command t))))

(defun elpaca--continue-build (e &rest args)
  "Run E's next build step.
Optional ARGS are passed to `elpaca--signal', which see."
  (when args (apply #'elpaca--signal e args))
  (unless (memq (elpaca--status e) elpaca--inactive-states)
    (if-let ((elpaca-queue-limit)
             ((not (elpaca<-builtp e)))
             ;; Don't double count current E
             (active (1- (cl-loop for (_ . e) in (elpaca-q<-elpacas (elpaca--q e))
                                  count (not (memq (elpaca--status e) elpaca--inactive-states)))))
             ((>= active elpaca-queue-limit)))
        (unless (eq (elpaca--status e) 'blocked) ;;@MAYBE: check for queue-throttled, too?
          (push 'queue-throttled (elpaca<-statuses e))
          (elpaca--signal e "elpaca-queue-limit exceeded" 'blocked nil 1))
      (let ((fn (or (pop (elpaca<-build-steps e)) #'elpaca--finalize)))
        (condition-case-unless-debug err ;;@TODO: signal/catch custom error types
            (funcall fn e)
          ((error) (elpaca--fail e (format "%s: %S" fn err))))))))

(defun elpaca--log-duration (e)
  "Return E's log duration."
  (time-subtract (nth 1 (car (elpaca<-log e))) (elpaca<-queue-time e)))

(defun elpaca--queue (order &optional queue)
  "ADD ORDER to QUEUE or current queue. Return E."
  (if-let ((item (elpaca--first order))
           ((not after-init-time))
           (e (elpaca-get item)))
      (if-let ((dependents (elpaca<-dependents e)))
          (warn "%S previously queued as dependency of: %S" item dependents)
        (warn "Duplicate item queued: %S" item))
    (let* ((e (elpaca<-create order))
           (log (pop (elpaca<-log e)))
           (status (car log))
           (info (nth 2 log))
           (q (or queue (car elpaca--queues))))
      (when queue (setf (elpaca<-queue-id e) (elpaca-q<-id q)
                        (elpaca<-init e) (elpaca-q<-type q)))
      (setf (alist-get (elpaca<-id e) (elpaca-q<-elpacas q)) e)
      (if (eq status 'struct-failed)
          (elpaca--fail e info)
        (elpaca--signal e info status nil 1))
      e)))

;;;###autoload
(defun elpaca-split-queue (&rest args)
  "Split remaining elpacas into new queue with ARGS."
  (let ((current (car elpaca--queues)))
    (unless (or (elpaca-q<-elpacas current) (elpaca-q<-forms current))
      (pop elpaca--queues)))
  (push (apply #'elpaca-q<-create args) elpaca--queues)
  nil)

;;;###autoload
(defmacro elpaca-queue (&rest body)
  "Execute BODY in new queue."
  (declare (debug t))
  `(progn (elpaca-split-queue) ,@body (elpaca-split-queue)))

(defvar elpaca--post-queues-hook nil)
(defun elpaca--finalize-queue (q)
  "Run Q's post installation functions:
- load cached autoloads
- evaluate deferred package configuration forms
- possibly run `elpaca-after-init-hook'."
  (when-let ((autoloads (elpaca-q<-autoloads q)))
    (condition-case-unless-debug err
        (eval `(progn ,@(reverse autoloads)) t)
      ((error) (warn "Autoload Error: %S" err))))
  (setf (elpaca-q<-status q) 'complete) ; Avoid loop when forms call elpaca-process-queue.
  (when-let ((forms (nreverse (elpaca-q<-forms q))))
    (cl-loop for (item . thunk) in forms
             do (condition-case-unless-debug err
                    (funcall thunk)
                  ((error) (warn "Config Error %s: %S" item err))))
    (setf (elpaca-q<-forms q) nil))
  (run-hooks 'elpaca-post-queue-hook)
  (let ((next (nth (1+ (elpaca-q<-id q)) (reverse elpaca--queues))))
    (unless (or elpaca-after-init-time ; Already run.
                elpaca--waiting ; Won't know if final queue until after waiting.
                (not (eq (elpaca-q<-type q) 'init)) ; Already run.
                (and next (eq (elpaca-q<-type next) 'init))) ; More init queues.
      (elpaca-split-queue)
      (remove-variable-watcher 'initial-buffer-choice #'elpaca--set-ibc)
      (setq elpaca-after-init-time (current-time) elpaca--ibs-set nil)
      (run-hooks 'elpaca-after-init-hook))
    (if (and next (or (elpaca-q<-elpacas next) (elpaca-q<-forms next)))
        (elpaca--process-queue next)
      (run-hooks 'elpaca--post-queues-hook))))

(defun elpaca--throttled-p (e)
  "Return t if E is blocked due to `elpaca-queue-limit'."
  (let ((statuses (elpaca<-statuses e)))
    (and (eq (car statuses) 'blocked) (eq (cadr statuses) 'queue-throttled))))

(defun elpaca--finalize (e)
  "Declare E finished or failed."
  (unless (eq (elpaca--status e) 'failed)
    (elpaca--signal
     e (concat  "✓ " (format-time-string "%s.%3N" (elpaca--log-duration e)) " secs")
     'finished))
  (let* ((q (elpaca--q e))
         (es (elpaca-q<-elpacas q))
         (next (and elpaca-queue-limit (cdr (cl-find-if #'elpaca--throttled-p es :key #'cdr)))))
    (when next
      (elpaca--signal next nil 'unthrottled)
      (elpaca--continue-build next))
    (when (= (cl-incf (elpaca-q<-processed q)) (length es)) (elpaca--finalize-queue q))))

(defun elpaca--command-string (strings &optional prefix)
  "Return string of form PREFIX STRINGS."
  (concat (or prefix "$") (string-join strings " ")))

(defun elpaca--call-with-log (e verbosity &rest command)
  "Call and Log E's COMMAND with VERBOSITY."
  (elpaca--signal e (elpaca--command-string command) nil nil verbosity)
  (apply #'elpaca-process-call command))

(defun elpaca--configure-remotes (e)
  "Add and/or rename E's repo remotes."
  (let ((fetchp nil))
    (when-let ((default-directory (elpaca<-repo-dir e))
               (recipe            (elpaca<-recipe   e))
               (remotes           (plist-get recipe :remotes)))
      (elpaca--signal e "Configuring Remotes" 'adding-remotes)
      (unless (ignore-errors (mapcar #'length remotes)) (setq remotes (list remotes)))
      (cl-loop with renamed for spec in remotes do
               (if (stringp spec)
                   (if renamed
                       (elpaca--signal e (format "ignoring :remotes rename %S" spec))
                     (unless (equal spec elpaca-default-remote-name)
                       (elpaca--call-with-log
                        e 1 "git" "remote" "rename" elpaca-default-remote-name spec))
                     (setq renamed spec))
                 (when-let ((remote    (car spec))
                            (props     (cdr spec))
                            (inherited (elpaca-merge-plists recipe props))
                            (URI       (elpaca--repo-uri inherited)))
                   (setq fetchp t)
                   (elpaca--call-with-log e 1 "git" "remote" "add" remote URI)))))
    (when fetchp (push #'elpaca--fetch (elpaca<-build-steps e))))
  (elpaca--continue-build e))

(defun elpaca--remove-build-steps (e spec)
  "Remove each step in SPEC from E."
  (setf (elpaca<-build-steps e) (cl-set-difference (elpaca<-build-steps e) spec)))

(defun elpaca--files (e &optional files nocons)
  "Return alist of E :files to be symlinked: (PATH . TARGET PATH).
FILES and NOCONS are used recursively."
  (let* ((repo-dir          (elpaca<-repo-dir e))
         (default-directory repo-dir)
         (build-dir         (elpaca<-build-dir e))
         (recipe            (elpaca<-recipe e))
         (files             (or files (if-let ((member (plist-member recipe :files)))
                                          (cadr member) elpaca-default-files-directive)))
         (exclusions        nil)
         (targets           nil)
         (with-subdirs      nil))
    (dolist (el files)
      (pcase el
        ((pred stringp) (push (or (file-expand-wildcards el) el) targets))
        (`(:exclude  . ,excluded)
         (push (elpaca--files e excluded 'nocons) exclusions)
         nil)
        (:defaults
         (push (elpaca--files e elpaca-default-files-directive 'nocons) targets))
        ;;@FIX: subdir needn't be same name as globbed path...
        (`(,_subdir . ,paths)
         (cl-loop for path in paths
                  for expanded = (file-expand-wildcards path)
                  do (cl-loop for path in expanded
                              do (push (cons (expand-file-name path repo-dir)
                                             (expand-file-name path build-dir))
                                       with-subdirs))))))
    (if nocons
        targets
      (append
       with-subdirs
       (cl-loop for target in (flatten-tree targets)
                unless (or (not (file-exists-p (expand-file-name target)))
                           (member target (flatten-tree exclusions)))
                collect
                (cons (expand-file-name target)
                      (expand-file-name (file-name-nondirectory target) build-dir)))))))

(defun elpaca--link-build-files (e)
  "Link E's :files into its builds subdirectory."
  (elpaca--signal e "Linking build files" 'linking)
  (let* ((build-dir (elpaca<-build-dir e))
         (files (or (elpaca<-files e)
                    (setf (elpaca<-files e) (elpaca--files e)))))
    (when (file-exists-p build-dir) (delete-directory build-dir 'recusrive))
    (make-directory build-dir 'parents)
    (dolist (spec files)
      (when-let ((file   (car spec))
                 ((file-exists-p file))
                 (link   (cdr spec)))
        (make-directory (file-name-directory link) 'parents)
        (make-symbolic-link file link 'overwrite))))
  (elpaca--continue-build e "Build files linked"))

(defun elpaca--add-info-path (e)
  "Add the E's info to `Info-directory-list'."
  (let ((build-dir (elpaca<-build-dir e)))
    (if (file-exists-p (expand-file-name "dir" build-dir))
        (progn
          (elpaca--signal e "Adding Info path" 'info)
          (with-eval-after-load 'info
            (info-initialize)
            (cl-pushnew build-dir Info-directory-list :test #'equal)))
      (elpaca--signal e "No Info dir file found" 'info))
    (elpaca--continue-build e)))

(defun elpaca--process-busy (process) ;;@TODO restore previous status when unblocked
  "Update E's status when PROCESS has stopped producing output."
  (when-let (((eq (process-status process) 'run))
             (e (process-get process :elpaca)))
    (elpaca--signal e (process-get process :parsed) 'busy)))

(defun elpaca--process-filter (process output)
  "Filter PROCESS OUTPUT."
  (process-put process :raw-output (concat (process-get process :raw-output) output))
  (let* ((e       (process-get process :elpaca))
         (parsed  (process-get process :parsed))
         (timer   (process-get process :timer))
         (chunk   (concat parsed output))
         (lines   (split-string chunk "\n"))
         (returnp (string-match-p "\r" chunk))
         (linep   (string-empty-p (car (last lines)))))
    (unless (process-get process :messaged)
      (elpaca--signal e (elpaca--command-string (process-command process)) nil nil 1)
      (process-put process :messaged t))
    (when timer (cancel-timer timer))
    (unless (eq (elpaca--status e) 'failed)
      (process-put process :timer (run-at-time elpaca-busy-interval nil
                                               #'elpaca--process-busy process)))
    (unless linep
      (process-put process :parsed (car (last lines)))
      (setq lines (butlast lines)))
    (dolist (line lines)
      (unless (string-empty-p line)
        (elpaca--signal e (car (last (split-string line "\r" t))) nil returnp)))))

(defun elpaca--process-sentinel (&optional info status process event)
  "Update E's INFO and STATUS when PROCESS EVENT is finished."
  (if-let ((e (process-get process :elpaca))
           ((and (equal event "finished\n") (not (eq (elpaca--status e) 'failed)))))
      (elpaca--continue-build e info status)
    (setf (car (car (elpaca<-log e))) 'failed)
    (elpaca--update-log-buffer)))

(defun elpaca--compile-info-process-sentinel (process event)
  "Sentinel for info compilation PROCESS EVENT."
  (let* ((e  (process-get process :elpaca))
         (finished (equal event "finished\n")))
    (unless finished
      (setf (elpaca<-build-steps e)
            (cl-set-difference (elpaca<-build-steps e) '(elpaca--install-info elpaca--add-info-path))))
    (elpaca--continue-build
     e (if finished "Info compiled" (concat "Compilation failure: " (string-trim event))))))

(defun elpaca--make-process (e &rest spec)
  "Attach process to E from `make-process' SPEC plist."
  (declare (indent 1))
  (let ((process (make-process
                  :name (concat "elpaca-" (plist-get spec :name) "-" (elpaca<-package e))
                  :connection-type (or (plist-get spec :connection-type) 'pipe)
                  :command (plist-get spec :command)
                  :filter (or (plist-get spec :filter) #'elpaca--process-filter)
                  :sentinel (plist-get spec :sentinel))))
    (process-put process :elpaca e)
    (setf (elpaca<-process e) process)))

(defun elpaca--compile-info (e)
  "Compile E's .texi files."
  (elpaca--signal e "Compiling Info files" 'info)
  (if-let ((default-directory (elpaca<-build-dir e))
           (elpaca-makeinfo-executable)
           (no-info t)
           (files
            (cl-loop for (repo-file . build-file) in
                     (or (elpaca<-files e)
                         (setf (elpaca<-files e) (elpaca--files e)))
                     when (and no-info (string-match-p "\\.info$" repo-file))
                     do (setq no-info nil)
                     for f = (when (string-match-p "\\.texi\\(nfo\\)?$" repo-file)
                               (list repo-file "-o"
                                     (concat (file-name-sans-extension build-file) ".info")))
                     when f collect f)))
      (elpaca--make-process e
        :name "compile-info"
        :command `(,elpaca-makeinfo-executable ,@(apply #'append files))
        :sentinel #'elpaca--compile-info-process-sentinel)
    (when no-info (elpaca--remove-build-steps e '(elpaca--install-info elpaca--add-info-path)))
    (elpaca--continue-build
     e (concat (if elpaca-makeinfo-executable "Info source files" "makeinfo") " not found"))))

(defun elpaca--install-info-process-sentinel (process event)
  "Sentinel for info installation PROCESS EVENT."
  (let ((e (process-get process :elpaca)))
    (elpaca--continue-build e (if (equal event "finished\n")
                                  "Info installed"
                                (concat "Failed to install Info: " (string-trim event))))))

(defun elpaca--install-info-async (file dir e)
  "Asynchronously Install E's .info FILE in Info DIR."
  (elpaca--signal e file)
  (let* ((default-directory (elpaca<-build-dir e)))
    (elpaca--make-process e
      :name "install-info"
      :command (list elpaca-install-info-executable file dir)
      :sentinel #'elpaca--install-info-process-sentinel)))

(defun elpaca--install-info (e)
  "Install E's .info files."
  (when-let ((elpaca-install-info-executable)
             (dir (expand-file-name "dir" (elpaca<-build-dir e)))
             ((not (file-exists-p dir)))
             (specs (or (elpaca<-files e) (setf (elpaca<-files e) (elpaca--files e)))))
    (elpaca--signal e "Installing Info files" 'info)
    (cl-loop for (target . link) in specs
             for file = (cond
                         ((string-match-p "\\.info$" link) link)
                         ((string-match-p "\\.texi\\(nfo\\)?$" target)
                          (concat (file-name-sans-extension link) ".info")))
             when (and file (file-exists-p file))
             do (setf (elpaca<-build-steps e)
                      (push (apply-partially #'elpaca--install-info-async file dir)
                            (elpaca<-build-steps e)))))
  (elpaca--continue-build e (unless elpaca-install-info-executable
                              "No elpaca-install-info-executable")))

(defun elpaca--dispatch-build-commands-process-sentinel (process event)
  "PROCESS EVENT."
  (let ((e    (process-get process :elpaca))
        (type (process-get process :build-type)))
    (cond
     ((equal event "finished\n") (elpaca--continue-build e (concat type " steps finished")))
     ((string-match-p "abnormally" event) (elpaca--fail e (concat type " command failed"))))))

(defun elpaca--dispatch-build-commands (e type)
  "Run E's TYPE commands for.
TYPE is either the keyword :pre-build, or :post-build.
Each command is either an elisp form to be evaluated or a list of
strings to be executed in a shell context of the form:

  (\"executable\" \"arg\"...)

Commands are executed in the E's repository directory.
The keyword's value is expected to be one of the following:

  - A single command
  - A list of commands
  - nil, in which case no commands are executed.
    Note if :build is nil, :pre/post-build commands are not executed."
  (if-let ((recipe   (elpaca<-recipe e))
           (commands (plist-get recipe type))
           (name (symbol-name type)))
      (progn
        (elpaca--signal e (concat "Running " name " commands") (intern (substring name 1)))
        (let* ((default-directory (elpaca<-repo-dir e))
               (emacs             (elpaca--emacs-path))
               (program           `(let ((load-prefer-newer t)
                                         (gc-cons-percentage 1.0))
                                     (require 'elpaca)
                                     (normal-top-level-add-subdirs-to-load-path)
                                     (elpaca--run-build-commands ',commands)))
               (process (elpaca--make-process e
                          :name name :connection-type 'pty
                          :command (list
                                    emacs "-Q"
                                    "-L" "./"
                                    "-L" (expand-file-name "elpaca/" elpaca-builds-directory)
                                    "--batch"
                                    "--eval" (let (print-level print-length print-circle)
                                               (format "%S" program)))
                          :sentinel #'elpaca--dispatch-build-commands-process-sentinel)))
          (process-put process :build-type name)))
    (elpaca--continue-build e)))

(defun elpaca--run-pre-build-commands (e)
  "Run E's :pre-build commands."
  (elpaca--dispatch-build-commands e :pre-build))

(defun elpaca--run-post-build-commands (e)
  "Run E's :post-build commands."
  (elpaca--dispatch-build-commands e :post-build))

;;@HACK: It seems like `directory-files-recursively' is a little slow because it
;;covers all sorts of general edge cases. e.g. tramp remote files. We shouldn't
;;need that here.
(defun elpaca--directory-files-recursively (directory regexp)
  "Return DIRECTORY files matching REGEXP."
  (let ((default-directory (expand-file-name directory)))
    (flatten-tree
     (cl-loop for file in (directory-files ".")
              unless (member file '("." ".." ".git"))
              collect (if (file-directory-p file)
                          (unless (file-symlink-p file)
                            (elpaca--directory-files-recursively file regexp))
                        (when (string-match-p regexp file) (expand-file-name file)))))))

(defun elpaca--dependencies (e &optional recache)
  "Return a list of E's declared dependencies.
If RECACHE is non-nil, do not use cached dependencies."
  (let ((cache (elpaca<--dependencies e)))
    (if-let
        (((or recache (not cache)))
         (default-directory (elpaca<-repo-dir e))
         (recipe (elpaca<-recipe e))
         ((or (file-exists-p default-directory) (error "Repository not on disk")))
         (package (file-name-sans-extension (elpaca<-package e)))
         (name (concat package ".el"))
         (regexp (concat "^" name "$"))
         (main (if-let ((declared (plist-member recipe :main)))
                   (or (cadr declared) (not (cadr declared)))
                 (or (cl-some (lambda (name) (let ((file (expand-file-name name)))
                                               (and (file-exists-p file) file)))
                              (list (concat package "-pkg.el")
                                    name
                                    (concat "./lisp/" name)
                                    (concat "./elisp/" name)))
                     (car (directory-files default-directory nil regexp))
                     (car (elpaca--directory-files-recursively default-directory regexp))
                     ;; Best guess if there is no file matching the package name...
                     (car (directory-files default-directory nil "\\.el\\'" 'nosort))
                     (error "Unable to find main elisp file for %S" package)))))
        (let ((deps
               (unless (eq main t)
                 (with-temp-buffer
                   (insert-file-contents-literally main)
                   (if (string-suffix-p "-pkg.el" main) (eval (nth 4 (read (current-buffer))))
                     (when-let
                         ((case-fold-search t)
                          ((re-search-forward
                            "^;+[ ]+\\(Package-Requires\\)[ ]*:[ ]*" nil 'noerror))
                          (deps (list (buffer-substring-no-properties (point) (line-end-position)))))
                       (forward-line 1)
                       (while (looking-at "^;+\\(\t\\|[\t\s]\\{2,\\}\\)\\(.+\\)")
                         (push (match-string-no-properties 2) deps)
                         (forward-line 1))
                       (condition-case err
                           (mapcar (lambda (d) (if (cdr-safe d) d (list (elpaca--first d) "0")))
                                   (read (string-join (nreverse deps) " ")))
                         ((error) (error "%S Package-Requires error: %S" main err)))))))))
          (setf (elpaca<--dependencies e) (or deps :nil))
          deps)
      (and (not (eq cache :nil)) cache))))

;;@DECOMPOSE: The body of this function is similar to `elpaca--clone-dependencies'.
;; Refactor into a macro to operate on dependencies?
(defun elpaca--queue-dependencies (e)
  "Queue E's dependencies."
  (elpaca--signal e "Queueing Dependencies" 'queueing-deps nil 1)
  (let ((queued (cl-loop
                 with queued = (elpaca--queued)
                 with e-id = (elpaca<-id e)
                 for (item . _) in (elpaca--dependencies e)
                 for d = (and (not (memq item elpaca-ignored-dependencies))
                              (or (elpaca-alist-get item queued)
                                  (elpaca--queue item)))
                 when (and d (eq (elpaca--status d) 'queued))
                 collect (prog1 d
                           (unless (memq item (elpaca<-dependencies e))
                             (push item (elpaca<-dependencies e)))
                           (unless (memq e-id (elpaca<-dependents d))
                             (push e-id (elpaca<-dependents d)))))))
    (if (not queued)
        (elpaca--continue-build e "No dependencies to queue" nil nil 1)
      ;; We do this in two steps so that e is aware of all its
      ;; dependencies before any single dependency starts its build.
      ;; Otherwise a dependency may finish prior to other dependencies being
      ;; registered. This will cause the dependent e to become unblocked
      ;; multiple times and run its build steps simultaneously/out of order.
      (elpaca--signal e nil 'blocked)
      (mapc #'elpaca--continue-build queued))))

;;@MAYBE: Package major version checks.
(defun elpaca--clone-dependencies (e)
  "Clone E's dependencies."
  (elpaca--signal e "Cloning Dependencies" 'blocked)
  (if-let ((dependencies (elpaca--dependencies e))
           (externals (cl-loop for dependency in dependencies
                               for item = (car dependency)
                               unless (memq item elpaca-ignored-dependencies)
                               collect item)))
      (if-let ((emacs (assoc 'emacs dependencies)) ;@TODO: check in prev loop?
               ((< emacs-major-version (truncate (string-to-number (cadr emacs))))))
          (elpaca--fail e (format "Requires %S; running %S" emacs emacs-version))
        (cl-loop with finished = 0
                 with pending = nil
                 with e-id = (elpaca<-id e)
                 with q = (elpaca--q e)
                 for dependency in externals
                 for queued = (elpaca-alist-get dependency (elpaca--queued))
                 for d = (or queued (elpaca--queue dependency q))
                 for d-id = (elpaca<-id d) do
                 (when (and queued (> (elpaca<-queue-id d) (elpaca<-queue-id e)))
                   (elpaca--fail d (format "dependent %S in past queue" e-id))
                   (elpaca--fail e (format "dependency %S in future queue" d-id)))
                 (unless (memq d-id (elpaca<-dependencies e))
                   (push d-id (elpaca<-dependencies e)))
                 (unless (memq e-id (elpaca<-dependents d))
                   (push e-id (elpaca<-dependents d)))
                 (unless (and queued (not (elpaca--throttled-p queued)))
                   (elpaca--signal e nil 'blocked)
                   (push 'requested-as-dependency (elpaca<-statuses d))
                   (push d pending))
                 (when (and queued (eq (elpaca--status queued) 'finished)) (cl-incf finished))
                 finally (if (= (length externals) finished)
                             (elpaca--continue-build e nil 'unblocked)
                           (mapc #'elpaca--continue-build pending))))
    (elpaca--continue-build e "No external dependencies detected" 'no-deps)))

(defun elpaca--remote-default-branch (remote)
  "Return REMOTE's \"default\" branch.
This is the branch that would be checked out upon cloning."
  (elpaca-with-process (elpaca-process-call "git" "remote" "show" remote)
    (if success
        (if (string-match "\\(?:[^z-a]*HEAD branch:[[:space:]]+\\([^z-a]*?\\)$\\)" stdout)
            (match-string 1 stdout)
          (error "Unable to determine remote default branch"))
      (error (format "Remote default branch error: %S" stderr)))))

(defun elpaca--checkout-ref (e)
  "Check out E's ref."
  (let* ((recipe (elpaca<-recipe e))
         (default-directory (elpaca<-repo-dir e))
         (remotes (plist-get recipe :remotes))
         (remote (let ((default (elpaca--remote remotes)))
                   (when (listp default) (setq recipe (elpaca-merge-plists recipe (cdr default))))
                   default))
         (ref    (plist-get recipe :ref))
         (tag    (plist-get recipe :tag))
         (branch (plist-get recipe :branch))
         (target (or ref tag branch)))
    (when-let ((name    (car-safe remote))
               (default (elpaca-process-output "git" "rev-parse" "--abbrev-ref" "HEAD")))
      (elpaca--call-with-log e 1 "git" "checkout" "--detach")
      (elpaca--call-with-log e 1 "git" "branch"   "--delete" (string-trim default))
      (elpaca--call-with-log e 1 "git" "config"   "checkout.defaultRemote" name)
      (when-let (((not branch))
                 (default-branch
                  (condition-case err ;;@FIX: will this stop if we fail elpaca?
                      (elpaca--remote-default-branch name)
                    (t (elpaca--fail e (format "Remote default branch err: %S" err))))))
        (setq branch default-branch target branch)))
    (if (null target)
        (unless (eq (elpaca--status e) 'failed)
          (elpaca--continue-build e nil 'ref-checked-out))
      (cond
       ((and ref (or branch tag))
        (elpaca--signal
         e (format ":ref %S overriding %S %S" ref (if branch :branch :tag) (or branch tag))))
       ((and tag branch)
        (elpaca--fail e (format "Ambiguous ref: :tag %S, :branch %S" tag branch))))
      (elpaca--signal e (concat "Checking out " target) 'checking-out-ref)
      (unless (eq (elpaca--status e) 'failed)
        (elpaca--make-process e
          :name "checkout-ref"
          :command
          `("git" "-c" "advice.detachedHead=false" ;ref, tag may detach HEAD
            ,@(cond
               (ref    (list "checkout" ref))
               (tag    (list "checkout" (concat "tags/" tag)))
               (branch (list "switch" "-C" branch ; "--no-guess"?
                             (concat (or (elpaca--first remote)
                                         elpaca-default-remote-name)
                                     "/" branch)))))
          :sentinel (apply-partially #'elpaca--process-sentinel
                                     (concat target " checked out")
                                     'ref-checked-out))))))

(defun elpaca--check-status (dependency e)
  "Possibly change E's status depending on DEPENDENCY statuses."
  (when-let ((e-status (elpaca--status e))
             ((not (eq e-status 'finished))))
    (cl-loop with failed
             with blocked
             with queued = (elpaca--queued)
             for d in (elpaca<-dependencies e)
             for found = (elpaca-alist-get d queued)
             for status = (elpaca--status found)
             unless (eq status 'finished)
             do (push d (if (eq status 'failed) failed blocked))
             finally
             (cond
              (failed (elpaca--fail e (format "Failed dependencies: %S" failed)))
              (blocked (elpaca--signal
                        e (concat "Blocked by dependencies: " (prin1-to-string blocked)) 'blocked))
              ((eq e-status 'blocked)
               (elpaca--continue-build
                e (concat "unblocked by dependency " (elpaca<-package dependency)) 'unblocked))))))

(defun elpaca--clone-process-sentinel (process _event)
  "Sentinel for clone PROCESS."
  (if-let ((e (process-get process :elpaca))
           (success (= (process-exit-status process) 0)))
      (elpaca--continue-build e)
    (if (or (memq 'reclone (elpaca<-statuses e))
            (not (plist-get (elpaca<-recipe e) :depth)))
        (elpaca--fail e (nth 2 (car (elpaca<-log e))))
      (setf (elpaca<-recipe e) (plist-put (elpaca<-recipe e) :depth nil))
      (elpaca--signal e "Re-cloning with recipe :depth nil" 'reclone)
      (push #'elpaca--clone (elpaca<-build-steps e))
      (elpaca--continue-build e))))

(defun elpaca--remote (remotes)
  "Return default remote from :REMOTES."
  (and remotes (if (ignore-errors (mapcar #'length remotes)) (car remotes) remotes)))

(defun elpaca--clone (e)
  "Clone E's repo to `elpaca-directory'."
  (let* ((recipe  (elpaca<-recipe   e))
         (depth   (plist-get recipe :depth))
         (repodir (elpaca<-repo-dir e))
         (URI     (elpaca--repo-uri recipe))
         (default-directory elpaca-directory)
         (command
          `("git" "clone"
            ;;@TODO: Some refs will need a full clone or specific branch.
            ,@(when (numberp depth)
                (if (plist-get recipe :ref)
                    (elpaca--signal e "Ignoring :depth in favor of :ref")
                  `("--depth" ,(number-to-string depth))))
            ;;@FIX: allow override
            ,@(when-let ((branch (plist-get recipe :branch)))
                `("--single-branch" "--branch" ,branch))
            ,@(when-let ((remote (elpaca--remote (plist-get recipe :remotes)))
                         ((not (stringp remote))))
                '("--no-checkout"))
            ,URI ,repodir)))
    (elpaca--signal e "Cloning" 'cloning)
    (elpaca--make-process e
      :name "clone" :command command :connection-type 'pty
      :sentinel #'elpaca--clone-process-sentinel)))

(defun elpaca-generate-autoloads (package dir)
  "Generate autoloads in DIR for PACKAGE."
  (or (require 'loaddefs-gen nil t) (require 'autoload))
  (let* ((default-directory dir)
         (name (concat package "-autoloads.el"))
         (output    (expand-file-name name dir))
         (generated-autoload-file output)
         (autoload-timestamps nil)
         (backup-inhibited t)
         (version-control 'never)
         (find-file-hook nil) ; Don't clobber recentf.
         (write-file-functions nil)
         (left-margin 0)) ; Prevent spurious parens in autoloads.
    (cond
     ((fboundp 'loaddefs-generate)
      (loaddefs-generate
       (cl-loop with seen
                for file in (elpaca--directory-files-recursively dir "\\.el$")
                for d = (file-name-directory file)
                unless (member d seen)
                collect d do (push d seen))
       name nil nil nil t)) ;; Emacs 29
     ((fboundp 'make-directory-autoloads) (make-directory-autoloads dir output))
     ((fboundp 'update-directory-autoloads) ;; Compatibility for Emacs < 28.1
      (with-no-warnings (update-directory-autoloads dir))))
    (when-let ((buf (find-buffer-visiting output))) (kill-buffer buf))
    name))

(defun elpaca--generate-autoloads-async (e)
  "Generate E's autoloads asynchronously."
  (let* ((package           (elpaca<-package  e))
         (default-directory (elpaca<-build-dir e))
         (elpaca            (expand-file-name "elpaca/" elpaca-repos-directory))
         (program           (let (print-level print-circle)
                              (format "%S" `(progn (setq gc-cons-percentage 1.0)
                                                   (elpaca-generate-autoloads
                                                    ,package ,default-directory))))))
    (elpaca--make-process e
      :name "autoloads"
      :command (list (elpaca--emacs-path) "-Q" "-L" elpaca
                     "-l" (expand-file-name "elpaca.el" elpaca)
                     "--batch" "--eval" program)
      :sentinel (apply-partially #'elpaca--process-sentinel "Autoloads Generated" nil))
    (elpaca--signal e (concat "Generating autoloads: " default-directory) 'autoloads)))

(defun elpaca--activate-package (e)
  "Activate E's package.
Adds package's build dir to `load-path'.
Loads or caches autoloads."
  (elpaca--signal e "Activating package" 'activation)
  (let* ((build-dir (elpaca<-build-dir e))
         (default-directory build-dir)
         (package           (elpaca<-package e))
         (autoloads         (expand-file-name (concat package "-autoloads.el"))))
    (cl-pushnew build-dir load-path :test #'equal)
    ;;@TODO: condition on a slot we set on the e to indicate cached recipe?
    (elpaca--signal e "Package build dir added to load-path")
    (when (file-exists-p autoloads)
      (if elpaca-cache-autoloads
          (let ((forms nil))
            (elpaca--signal e "Caching autoloads")
            (with-temp-buffer
              (insert-file-contents autoloads)
              (goto-char (point-min))
              (condition-case _
                  (while t (push (read (current-buffer)) forms))
                ((end-of-file)))
              (push `(let ((load-file-name ,autoloads)
                           (load-in-progress t))
                       (condition-case err
                           (progn ,@(nreverse forms))
                         ((error) (warn "Error loading %S autoloads: %S" ,package err))))
                    (elpaca-q<-autoloads (elpaca--q e))))
            (elpaca--signal e "Autoloads cached"))
        (condition-case err
            (progn
              (load autoloads nil 'nomessage)
              (elpaca--signal e "Package activated" 'activated))
          ((error) (elpaca--signal
                    e (format "Failed to load %S: %S" autoloads err) 'failed-to-activate))))))
  (elpaca--continue-build e))

(defun elpaca--byte-compile (e)
  "Byte compile E's package."
  ;; Assumes all dependencies are 'built
  (elpaca--signal e "Byte compiling" 'byte-compilation)
  (let* ((default-directory (elpaca<-build-dir e))
         (emacs             (elpaca--emacs-path))
         (dependency-dirs
          (cl-loop for dep in (elpaca-dependencies (elpaca<-id e) '(emacs))
                   for item = (elpaca-get dep)
                   for build-dir = (and item (elpaca<-build-dir item))
                   when build-dir collect build-dir))
         (program `(let ((gc-cons-percentage 1.0)) ;; trade memory for gc speed
                     (dolist (dir ',(cons default-directory dependency-dirs))
                       (let ((default-directory dir))
                         (add-to-list 'load-path dir)
                         (normal-top-level-add-subdirs-to-load-path)))
                     (byte-recompile-directory ,default-directory 0 'force)))
         (print-level nil)
         (print-circle nil))
    (elpaca--make-process e
      :name "byte-compile"
      :command  `(,emacs "-Q" "--batch" "--eval" ,(format "%S" program))
      :sentinel (apply-partially #'elpaca--process-sentinel "Byte compilation complete" nil))))

;;;###autoload
(defun elpaca-dependencies (item &optional ignore interactive recurse)
  "Return recursive list of ITEM's dependencies.
IGNORE may be a list of symbols which are not included in the resulting list.
RECURSE is used to track recursive calls.
When INTERACTIVE is non-nil, message the list of dependencies."
  (interactive (list (elpaca--read-queued "Dependencies of: ") nil t))
  (if-let ((e (elpaca-get item))
           (dependencies (elpaca--dependencies e))
           (transitives (cl-loop for (d . _) in dependencies
                                 unless (memq d ignore) collect
                                 (cons d (elpaca-dependencies d (cons d ignore) nil t))))
           (deps (delete-dups (flatten-tree transitives))))
      (if interactive (message "%s" deps) deps)
    (when recurse item)))

(defun elpaca--dependents (item &optional noerror)
  "Return list of packages which depend on ITEM.
If NOERROR is non-nil, ignore E's for which dependencies cannot be determined."
  (delete-dups (cl-loop for (i . _) in (elpaca--queued)
                        for deps = (if noerror (ignore-errors (elpaca-dependencies i))
                                     (elpaca-dependencies i))
                        when (memq item deps) collect i)))
;;;###autoload
(defun elpaca-dependents (item &optional message)
  "Return recursive list of packages which depend on ITEM.
When MESSAGE is non-nil, message the list of dependents."
  (interactive (list (elpaca--read-queued
                      "Dependents of: "
                      (cl-remove-if-not #'elpaca--dependents (elpaca--queued) :key #'car))
                     t))
  (if message (message "%S" (elpaca--dependents item)) (elpaca--dependents item)))

(defcustom elpaca-interactive-interval 0.15
  "Time to wait before processing queues when multiple `elpaca' forms evaluated."
  :type 'number)
(defvar elpaca--interactive-timer nil
  "Debounces interactive evaluation of multiple `elpaca' forms.")

;;;; COMMANDS/MACROS
(defun elpaca--expand (order body make-thunk)
  "Expand `elpaca' and alike.
ORDER and BODY are as in `elpaca', while MAKE-THUNK is used to produce
the thunk when given BODY."
  (declare (side-effect-free t))
  (let ((o (gensym "order-")) (item (gensym "item-")) (q (gensym "q-")))
    `(let* ((,o ,@(if (memq (car-safe order) '(quote \`)) `(,order) `(',order)))
            (,item (elpaca--first ,o))
            (,q (or (and after-init-time (elpaca--q (elpaca-get ,item))) (car elpaca--queues))))
       ,@(when body
           (let ((thunk (funcall make-thunk body)))
             `((if ,item
                   (setf (alist-get ,item (elpaca-q<-forms ,q)) ,thunk)
                 ;;@FIX: nil semantics not good for multiple deferred...
                 (push (cons ,item ,thunk) (elpaca-q<-forms ,q))))))
       (when ,o (elpaca--queue ,o ,q))
       (when after-init-time
         (when-let ((e (elpaca-get ,item)))
           (elpaca--maybe-log)
           (elpaca--unprocess e)
           (push 'queued (elpaca<-statuses e)))
         (when (member this-command '(eval-last-sexp eval-defun)) (elpaca-process-queues))
         (when (member this-command '(eval-region eval-buffer org-ctrl-c-ctrl-c))
           (when elpaca--interactive-timer (cancel-timer elpaca--interactive-timer))
           (run-at-time elpaca-interactive-interval nil #'elpaca-process-queues)))
       nil)))

;;;###autoload
(defmacro elpaca (order &rest body)
  "Queue ORDER for installation/activation, defer execution of BODY.
If ORDER is `nil`, defer BODY until orders have been processed."
  (declare (indent 1) (debug t))
  (elpaca--expand
   order body
   (lambda (body) `(lambda () (eval '(progn ,@body) t)))))

;;;###autoload
(defmacro elpaca-thunk (order &rest body)
  "Queue ORDER like `elpaca', but BODY is wrapped in a thunk."
  (declare (indent 1) (debug t))
  (elpaca--expand
   order body
   (lambda (body) `(lambda () ,@body))))

(defcustom elpaca-wait-interval 0.01 "Seconds between `elpaca-wait' status checks."
  :type 'number)

(defun elpaca--dont-clear-message () "Block message clearing." 'dont-clear-message)
;;;###autoload
(defun elpaca-wait ()
  "Block until currently queued orders are processed.
When quit with \\[keyboard-quit], running sub-processes are not stopped."
  (when-let ((q (cl-find-if (lambda (q) (and (eq (elpaca-q<-status q) 'incomplete)
                                             (or (elpaca-q<-elpacas q) (elpaca-q<-forms q))))
                            elpaca--queues)))
    (setq elpaca--waiting t)
    (unless (or elpaca-after-init-time (not elpaca--ibs-set))
      (elpaca--maybe-log)
      (sit-for elpaca-wait-interval))
    (elpaca-process-queues)
    (condition-case nil
        (while (not (eq (elpaca-q<-status q) 'complete))
          (discard-input)
          (sit-for elpaca-wait-interval))
      (quit (mapcar (lambda (it) (let ((e (cdr it))) (unless (eq (elpaca--status e) 'finished)
                                                       (elpaca--fail e "User quit"))))
                    (elpaca-q<-elpacas q))))
    (elpaca-split-queue)
    (setq elpaca--waiting nil)))

(defvar elpaca--try-package-history nil "History for `elpaca-try'.")
;;;###autoload
(defun elpaca-try (order &optional interactive)
  "Try ORDER.
Install the repo/build files on disk.
Activate the corresponding package for the current session.
ORDER's package is not made available during subsequent sessions.
When INTERACTIVE is non-nil, immediately process ORDER, otherwise queue ORDER."
  (interactive
   (list (if (equal current-prefix-arg '(4))
             (minibuffer-with-setup-hook #'backward-char
               (read (read-string "elpaca-try: " "()" 'elpaca--try-package-history)))
           (if-let ((items (cl-remove-if #'elpaca-get (elpaca--menu-items t) :key #'car))
                    (item (elpaca-menu-item t items)))
               (cons (car item) (plist-get (cdr item) :recipe))
             (user-error "No menu item")))
         t))
  (if (not interactive)
      (elpaca--queue (elpaca--first order))
    (elpaca--maybe-log)
    (elpaca-queue
     (eval `(elpaca ,order
              ,(when-let (((equal current-prefix-arg '(4)))
                          ((listp order))
                          (id (pop order)))
                 ;;@TODO: implement as a proper menu function?
                 ;;What would the semantics of an update be here?
                 `(progn
                    (setf (alist-get ',id elpaca--menu-items-cache)
                          (list :source "elpaca-try"
                                :description "user-provided recipe"
                                :recipe '(:package ,(symbol-name id) ,@order)))
                    (elpaca--write-menu-cache))))
           t))
    (elpaca--process-queue (nth 1 elpaca--queues)))
  nil)

(defun elpaca--process (e)
  "Process E."
  (when (eq (elpaca--status e) 'queued) (elpaca--continue-build e)))

(defun elpaca--process-queue (q)
  "Process elpacas in Q."
  (cond
   ((eq (elpaca-q<-status q) 'complete)
    (when-let ((next (nth (1+ (elpaca-q<-id q)) (reverse elpaca--queues))))
      (elpaca--process-queue next)))
   ((and (not (elpaca-q<-elpacas q)) (elpaca-q<-forms q)) (elpaca--finalize-queue q))
   (t (mapc #'elpaca--process (reverse (mapcar #'cdr (elpaca-q<-elpacas q)))))))

(defun elpaca--maybe-reset-queue (q)
  "Reset Q containing incomplete orders."
  (let* ((elpacas (elpaca-q<-elpacas q))
         (processed (cl-count 'finished elpacas
                              :key (lambda (qd) (elpaca--status (cdr qd))))))
    (setf (elpaca-q<-processed q) processed
          (elpaca-q<-status q) (if (and (> processed 0) (= processed (length elpacas)))
                                   'complete 'incomplete))))

;;;###autoload
(defun elpaca-process-queues (&optional filter)
  "Process the incomplete queues.
FILTER must be a unary function which accepts and returns a queue list."
  (when (and after-init-time elpaca--debug-init) (setq debug-on-error t elpaca--debug-init nil))
  (if-let ((queues (if filter (funcall filter (reverse elpaca--queues))
                     (reverse elpaca--queues)))
           ((mapc #'elpaca--maybe-reset-queue queues))
           (incomplete (cl-find 'incomplete queues :key #'elpaca-q<-status)))
      (progn (setq elpaca--status-counts (elpaca--count-statuses))
             (elpaca--process-queue incomplete))
    (run-hooks 'elpaca--post-queues-hook)))

(defun elpaca--on-disk-p (item)
  "Return t if ITEM has an associated E and a build or repo dir on disk."
  (when-let ((e (elpaca-get item)))
    (or (file-exists-p (elpaca<-repo-dir e)) (file-exists-p (elpaca<-build-dir e)))))

;;@MAYBE: Should this delete user's declared package if it is a dependency?
;;@MAYBE: user option for deletion policy when repo is dirty.
;;;###autoload
(defun elpaca-delete (item &optional force deps ignored)
  "Remove a package associated with ITEM from cache and disk.
If DEPS is non-nil (interactively with \\[universal-argument]) delete dependencies.
If FORCE is non-nil (interactively with \\[universal-argument] \\[universal-argument])
do not confirm before deleting package and DEPS."
  (interactive (list (elpaca--read-queued "Delete Package: ")
                     (equal current-prefix-arg '(16))
                     (member current-prefix-arg '((4) (16)))))
  (let* ((e (let ((e (or (elpaca-get item) (elpaca<-create item))))
              (if (equal (elpaca--status e) 'struct-failed) ;; Orphaned packages
                  (setf (alist-get item (elpaca-q<-elpacas (elpaca--q e)))
                        (elpaca<-create (list item :url (symbol-name item))))
                e)))
         (repo-dir     (elpaca<-repo-dir e))
         (build-dir    (elpaca<-build-dir e))
         (dependents   (elpaca-dependents item))
         (dependencies (and deps (ignore-errors (elpaca-dependencies
                                                 item elpaca-ignored-dependencies)))))
    (when (cl-some #'elpaca--on-disk-p dependents)
      (user-error "Cannot delete %S unless dependents %S are deleted" item dependents))
    (when (or force (yes-or-no-p (format "Delete package %S? " item)))
      (cl-assert (not (member repo-dir (list user-emacs-directory elpaca-builds-directory
                                             elpaca-repos-directory))))
      (when (file-exists-p repo-dir) (delete-directory repo-dir 'recursive))
      (when (file-exists-p build-dir)
        (setq load-path (delete build-dir load-path))
        (delete-directory build-dir 'recursive))
      (dolist (queue elpaca--queues)
        (setf (elpaca-q<-elpacas queue)
              (cl-remove item (elpaca-q<-elpacas queue) :key #'car)))
      (setf (alist-get (car (cl-find (file-name-base (directory-file-name repo-dir))
                                     elpaca--repo-dirs :key #'cadr :test #'equal))
                       elpaca--repo-dirs nil t)
            nil)
      (message "Deleted package %S" item)
      (dolist (dependency dependencies nil)
        (elpaca-delete dependency 'force deps (push item ignored))))))

(defun elpaca--file-package (&optional file)
  "Return queued E if current buffer's FILE is part of a repo, nil otherwise."
  (when-let ((name (or file (buffer-file-name))))
    (cl-find-if (lambda (e) (assoc name (elpaca--files e)))
                (reverse (elpaca--queued)) :key #'cdr)))

(defun elpaca--read-queued (&optional prompt queued)
  "Return QUEUED item.
If PROMPT is non-nil, it is used instead of the default."
  (intern (completing-read
           (or prompt "Queued item: ")
           (sort (cl-delete-duplicates (mapcar #'car (or queued (elpaca--queued)))) #'string<)
           nil t)))

(defun elpaca--unprocess (e)
  "Mark E as unprocessed in its queue."
  (let ((q (elpaca--q e)))
    (setf (elpaca<-statuses e) nil
          (elpaca<-builtp e) nil
          (elpaca<-queue-time e) (current-time))
    (when (> (elpaca-q<-processed q) 0) (cl-decf (elpaca-q<-processed q)))
    (setf (elpaca-q<-status q) 'incomplete)))

;;;###autoload
(defun elpaca-rebuild (item &optional interactive)
  "Rebuild ITEM's associated package.
When INTERACTIVE is non-nil, prompt for ITEM, immediately process.
With a prefix argument, rebuild current file's package or prompt if none found."
  (interactive (list (or (and-let* ((current-prefix-arg)
                                    (queued (elpaca--file-package))
                                    ((car queued))))
                         (elpaca--read-queued "Rebuild package: "))
                     t))
  (let ((e (or (elpaca-get item) (user-error "Package %S is not queued" item))))
    (when (eq (elpaca--status e) 'finished)
      ;;@MAYBE: remove Info/load-path entries?
      (setf (elpaca<-build-steps e)
            (cl-set-difference (elpaca--build-steps (elpaca<-recipe e))
                               '(elpaca--clone
                                 elpaca--configure-remotes
                                 elpaca--fetch
                                 elpaca--checkout-ref
                                 elpaca--clone-dependencies
                                 elpaca--activate-package))))
    (elpaca--unprocess e)
    (elpaca--signal e "Rebuilding" 'queued)
    (setf elpaca-cache-autoloads nil (elpaca<-files e) nil)
    (when interactive
      (elpaca--maybe-log)
      (elpaca-process-queues))))

(defun elpaca--log-updates (e)
  "Log E's fetched commits."
  (elpaca--signal e nil 'update-log)
  (let* ((default-directory (elpaca<-repo-dir e))
         (date (string-trim (elpaca-process-output "git" "show" "-s" "--format=%ci"))))
    (elpaca--make-process e
      :name "log-updates"
      ;; Pager breaks pipe process.
      :command (list "git" "--no-pager" "log" "--reverse" (concat "--since=" date)
                     "--pretty=%h %s (%ch)" "..@{u}")
      :sentinel (apply-partially #'elpaca--process-sentinel nil nil))))

(defun elpaca--fetch (e)
  "Fetch E's remotes' commits."
  (elpaca--signal e nil 'fetching-remotes)
  (let* ((default-directory (elpaca<-repo-dir e)))
    (elpaca--make-process e
      :name "fetch"
      :command  '("git" "fetch" "--all" "-v") ;;@TODO: make --all optional
      :sentinel (apply-partially #'elpaca--process-sentinel "Remotes fetched" nil))))

;;;###autoload
(defun elpaca-fetch (item &optional interactive)
  "Fetch ITEM's associated package remote commits.
This does not merge changes or rebuild the packages.
If INTERACTIVE is non-nil immediately process, otherwise queue."
  (interactive (list (elpaca--read-queued "Fetch Package Updates: ") t))
  (let ((e (or (elpaca-get item) (user-error "Package %S is not queued" item))))
    (elpaca--unprocess e)
    (elpaca--signal e "Fetching updates" 'queued)
    (setf (elpaca<-build-steps e) (list #'elpaca--fetch #'elpaca--log-updates))
    (when interactive
      (elpaca--maybe-log)
      (elpaca--process e))))

;;;###autoload
(defun elpaca-fetch-all (&optional interactive)
  "Fetch queued elpaca remotes. If INTERACTIVE is non-nil, process queues."
  (interactive (list t))
  (when interactive (elpaca--maybe-log))
  (cl-loop for q in elpaca--queues
           do (setf (elpaca-q<-processed q) 0 (elpaca-q<-status q) 'incomplete))
  (cl-loop for (_ . e) in (reverse (elpaca--queued)) do
           (setf (elpaca<-build-steps e)
                 '(elpaca--queue-dependencies elpaca--fetch elpaca--log-updates)
                 (elpaca<-queue-time e) (current-time)
                 (elpaca<-statuses e) (list 'queued)
                 (elpaca<-builtp e) nil))
  (when interactive (elpaca-process-queues)))

(defun elpaca--merge-process-sentinel (process _event)
  "Handle PROCESS EVENT."
  (if-let (((= (process-exit-status process) 0))
           (e (process-get process :elpaca))
           (default-directory (elpaca<-repo-dir e)))
      (progn (when (equal (elpaca-process-output "git" "rev-parse" "HEAD")
                          (process-get process :elpaca-git-rev))
               (setf (elpaca<-build-steps e) nil))
             (elpaca--continue-build e))
    (elpaca--fail e)))

(defun elpaca--merge (e)
  "Merge E's fetched commits."
  (let* ((default-directory (elpaca<-repo-dir e))
         (rev (elpaca-process-output "git" "rev-parse" "HEAD")))
    (process-put (elpaca--make-process e
                   :name "merge"
                   :command  '("git" "merge" "--ff-only")
                   :sentinel #'elpaca--merge-process-sentinel)
                 :elpaca-git-rev rev)
    (elpaca--signal e "Merging updates" 'merging)))

(defun elpaca--announce-pin (e)
  "Dummy build step to announce a E's package is pinned."
  (elpaca--continue-build e "Skipping pinned package" 'pinned))

;;;###autoload
(defun elpaca-update (item &optional interactive)
  "Update ITEM's associated package.
If INTERACTIVE is non-nil, the queued order is processed immediately."
  (interactive (list (elpaca--read-queued "Update package: ") t))
  (let* ((e (or (elpaca-get item) (user-error "Package %S is not queued" item)))
         (recipe (elpaca<-recipe e))
         (pin (plist-get recipe :pin)))
    (elpaca--unprocess e)
    (setf (elpaca<-build-steps e)
          (if pin
              (list #'elpaca--announce-pin)
            `(elpaca--fetch
              elpaca--log-updates
              elpaca--merge
              ,@(cl-set-difference
                 (elpaca--build-steps recipe nil 'cloned (elpaca<-mono-repo e))
                 '(elpaca--configure-remotes
                   elpaca--fetch
                   elpaca--checkout-ref
                   elpaca--clone-dependencies
                   elpaca--activate-package))))
          (elpaca<-statuses e) (list 'queued))
    (when interactive
      (elpaca--maybe-log)
      (elpaca--process e))))

;;;###autoload
(defun elpaca-update-all (&optional interactive)
  "Update all queued packages. If INTERACTIVE is non-nil, process queues."
  (interactive (list t))
  (when interactive (elpaca--maybe-log))
  (cl-loop for q in elpaca--queues
           do (setf (elpaca-q<-processed q) 0 (elpaca-q<-status q) 'incomplete))
  (cl-loop with (seen repos)
           with ignored = (remove 'elpaca elpaca-ignored-dependencies)
           for (id . e) in (elpaca--queued)
           unless (memq id seen) do
           (let* ((repo (elpaca<-repo-dir e))
                  (deps (elpaca-dependencies id ignored))
                  (recipe (elpaca<-recipe e)))
             (cl-loop for dep in deps do (cl-pushnew id (elpaca<-dependents (elpaca-get dep))))
             (setf (elpaca<-build-steps e) nil)
             (cond
              ((plist-get recipe :pin) (elpaca--signal e "Skipping pinned repo" 'queued))
              ;;@FIX: we still want to rebuild after the repo has been pulled.
              ((member repo repos) (elpaca--signal e "Skipping mono-repo" 'queued))
              (t (setf (elpaca<-build-steps e)
                       `(elpaca--fetch
                         elpaca--log-updates
                         elpaca--merge
                         ,@(cl-set-difference
                            (elpaca--build-steps recipe nil 'cloned (elpaca<-mono-repo e))
                            '(elpaca--configure-remotes
                              elpaca--fetch
                              elpaca--checkout-ref
                              elpaca--clone-dependencies
                              elpaca--activate-package)))
                       (elpaca<-queue-time e) (current-time)
                       (elpaca<-statuses e) (list 'queued)
                       (elpaca<-builtp e) nil)
                 (elpaca--signal e (concat "Fetching " (if deps "dependencies" "remotes"))
                                 (when deps 'blocked))))
             (push id seen)
             (push repo repos)))
  (when interactive (elpaca-process-queues)))

;;; Lockfiles
(defun elpaca-declared-p (item)
  "Return t if ITEM is declared in user's init file, nil otherwise."
  (when-let ((e (elpaca-get item)))
    (or (elpaca<-init e) (cl-loop for dependent in (elpaca-dependents item)
                                  when (elpaca-declared-p dependent) return t))))

(defun elpaca-installed-p (item)
  "Return t if ITEM's associated repo directory is on disk, nil otherwise."
  (and-let* ((e (elpaca-get item))
             (repo-dir (elpaca<-repo-dir e))
             ((file-exists-p repo-dir)))))

(defun elpaca-worktree-dirty-p (item)
  "Return t if ITEM's associated repository has a dirty worktree, nil otherwise."
  (when-let ((e (elpaca-get item))
             (recipe (elpaca<-recipe e))
             (repo-dir (elpaca<-repo-dir e))
             ((file-exists-p repo-dir))
             (default-directory repo-dir))
    (not (string-empty-p (elpaca-process-output
                          "git" "-c" "status.branch=false" "status" "--short")))))

(defun elpaca-load-lockfile (&optional lockfile _force)
  "Load LOCKFILE. If FORCE is non-nil, @TODO."
  (interactive "fLockfile: ")
  (message "%S" lockfile))

(defun elpaca-write-lockfile (path)
  "Write lockfile to PATH for current state of package repositories."
  (interactive "FWrite lockfile to: ")
  (elpaca--write-file path
    (pp (nreverse
         (cl-loop with seen
                  for (item . e) in (elpaca--queued)
                  unless (member item seen)
                  for rev =
                  (let ((default-directory (elpaca<-repo-dir e)))
                    (elpaca-with-process
                        (elpaca-process-call "git" "rev-parse" "HEAD")
                      (if success
                          (string-trim stdout)
                        (error "Unable to write lockfile: %s %S" item stderr))))
                  for recipe = (copy-tree (elpaca<-recipe e))
                  do (setq recipe (plist-put recipe :ref rev))
                  ;;@MAYBE: recipe (plist-put recipe :pin t))
                  collect (cons item (list :source "lockfile"
                                           :date (current-time)
                                           :recipe recipe))
                  do (push item seen))))))

;;;###autoload
(defmacro elpaca-with-dir (item type &rest body)
  "Set `default-directory' for duration of BODY.
TYPE is either `repo' or `build' for ITEM's repo or build directory."
  (declare (indent 2) (debug t))
  `(let* ((e (elpaca-get ,item))
          (default-directory (,(intern (format "elpaca<-%s-dir" (symbol-name type))) e)))
     ,@body))

(declare-function elpaca-ui-current-package "elpaca-ui")
;;;###autoload
(defun elpaca-visit (&optional item build)
  "Open ITEM's local repository directory.
When BUILD is non-nil visit ITEM's build directory."
  (interactive (list (elpaca--read-queued
                      (format "Visit %s dir: " (if current-prefix-arg "build" "repo")))
                     current-prefix-arg))
  (when (eq item '##) (setq item nil)) ; Empty `elpaca--read-queued' response
  (if (not item)
      (find-file (if build elpaca-builds-directory elpaca-repos-directory))
    (if-let ((e (elpaca-get item))
             (dir (if build (elpaca<-build-dir e) (elpaca<-repo-dir e))))
        (if (file-exists-p dir)
            (find-file dir)
          (user-error "Directory does not exist: %S" dir))
      (user-error "%S is not a queued package" item))))

(defun elpaca--fallback-date (e)
  "Return time of last modification for E's built elisp, otherwise nil."
  (file-attribute-modification-time
   (file-attributes (expand-file-name (concat (elpaca<-package e) ".el")
                                      (elpaca<-build-dir e)))))

(defun elpaca--custom-candidates (&optional notry)
  "Return declared candidate list with no recipe in `elpaca-menu-functions'.
If NOTRY is non-nil do not include `elpaca-try' recipes."
  (cl-loop with seen
           for (item . e) in (elpaca--queued)
           for menu-item = (elpaca-menu-item item)
           for tried = (equal (plist-get (cdr menu-item) :source) "elpaca-try")
           unless (or (member item seen) (and menu-item (or (not tried) notry)))
           collect (if tried menu-item
                     (list item :source "Init file"
                           :date (ignore-errors (elpaca--fallback-date e))
                           :recipe (elpaca<-recipe e)
                           :description "Not available in menu functions"))
           do (push item seen)))

;;;###autoload
(defun elpaca-browse (item)
  "Browse ITEM's :url."
  (interactive (list (let* ((elpaca-overriding-prompt "Browse package: ")
                            (recipe (elpaca-recipe nil (append (elpaca--custom-candidates)
                                                               (elpaca--menu-items t)))))
                       (intern (plist-get recipe :package)))))
  (if-let ((found (or (elpaca-get item)
                      (alist-get item (elpaca--menu-items t))
                      (alist-get item (elpaca--custom-candidates))))
           (url (or (plist-get found :url)
                    (file-name-sans-extension
                     (elpaca--repo-uri (elpaca-merge-plists
                                        (or (plist-get found :recipe)
                                            (and (elpaca<-p found) (elpaca<-recipe found))
                                            (user-error "No URL associated with item %S" item))
                                        '(:protocol https)))))))
      (browse-url url)))

;;;###autoload
(defun elpaca-version (&optional output)
  "Return Elpaca version information string.
OUTPUT may be any of the following:
  - nil Return raw alist of form ((category . info) ...)
  - `string' Return formmatted string.
  - `message' (used when called interactively) to message formatted string."
  (interactive '(message))
  (let* ((default-directory (expand-file-name "elpaca/" elpaca-repos-directory))
         (git  (string-trim (elpaca-process-output "git" "--version")))
         (repo (string-trim (elpaca-process-output "git" "log" "--pretty=%h %D" "-1")))
         (info (list (cons 'elpaca repo) (cons 'isntaller elpaca-installer-version)
                     (cons 'emacs (emacs-version)) (cons 'git git))))
    (when (member output '(message string))
      (setq info (format "Elpaca %s\ninstaller:      %S\nemacs-version:  %s\ngit --version:  %s"
                         repo elpaca-installer-version (emacs-version) git)))
    (if (eq output 'message) (message "%s" info) info)))

(provide 'elpaca)
;;; elpaca.el ends here
