;;; elpaca.el --- An elisp package manager           -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025  Nicholas Vollmer

;; Author: Nicholas Vollmer
;; URL: https://github.com/progfolio/elpaca
;; Created: Jan 1, 2022
;; Keywords: tools, convenience, lisp
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.0.2

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
(or noninteractive (= elpaca-installer-version 0.11)
    (lwarn '(elpaca installer) :warning "%s installer version does not match %s."
           (or (symbol-file 'elpaca-installer-version 'defvar) "Init")
           (expand-file-name "./doc/installer.el" (file-name-directory (or load-file-name (buffer-file-name))))))
(or (executable-find "git") (error "Elpaca unable to find git executable"))
(and (not after-init-time) load-file-name (featurep 'package) (warn "Package.el loaded before Elpaca"))

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

(defvar elpaca-after-init-time nil "`current-time' after processing all queues.")
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
                                elpaca--queue-dependencies
                                elpaca--check-version
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
    "docs/dir" "docs/*.info" "docs/*.texi" "docs/*.texinfo"
    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
              "README*" "*-pkg.el"))
  "Default value for the `:files' directive in recipes.
It is also spliced in at any point where the `:defaults' keyword
is used in a `:files' directive.")

(defvar elpaca-order-defaults (list :protocol 'https :inherit t :depth 'treeless)
  "Default order modifications.")

(defun elpaca-order-defaults (_order) "Return order defaults." elpaca-order-defaults)

(defcustom elpaca-order-functions '(elpaca-order-defaults)
  "Abnormal hook run to alter orders.
Each element must be a unary function which accepts an order.
An order may be a symbol naming a package, or list of the form (ID . PROPS...).
The function may return nil or a plist to be merged with the order.
This hook is run via `run-hook-with-args-until-success'."
  :type 'hook)

(defcustom elpaca-recipe-functions nil
  "Abnormal hook run to alter recipes.
Each element must be a unary function which accepts an recipe plist.
The function may return nil or a plist to be merged with the recipe.
This hook is run via `run-hook-with-args-until-success'."
  :type 'hook)

(defsubst elpaca-alist-get (key alist &optional default)
  "Return KEY's value in ALIST or DEFAULT.
Simplified, faster version of `alist-get'."
  (or (cdr (assq key alist)) default))

(defvar elpaca-menu-extensions--cache nil "Cache for `elpaca-menu-extenions' items.")
(defun elpaca-menu-extensions (request &optional item)
  "Return menu ITEM REQUEST."
  (if-let* (((eq request 'index))
            (cache (or elpaca-menu-extensions--cache (elpaca-menu-extensions 'update))))
      (if item (elpaca-alist-get item cache) cache)
    (setq elpaca-menu-extensions--cache
          (list (cons 'elpaca-use-package
                      (list :source "Elpaca extensions"
                            :description "Elpaca use-package support."
                            :recipe (list :package "elpaca-use-package" :wait t
                                          :repo "https://github.com/progfolio/elpaca.git"
                                          :files '("extensions/elpaca-use-package.el")
                                          :main "extensions/elpaca-use-package.el"
                                          :build '(:not elpaca--compile-info))))))))

(defcustom elpaca-menu-functions
  '( elpaca-menu-lock-file elpaca-menu-extensions elpaca-menu-org elpaca-menu-melpa
     elpaca-menu-non-gnu-elpa elpaca-menu-gnu-elpa elpaca-menu-declarations)
  "Abnormal hook to lookup packages in menus.
Each function is passed a request, which may be any of the following symbols:
  - `index`
     Must return a alist of the menu's package candidates.
     Each candidate is a cell of form:
     (PACKAGE-NAME . (:source SOURCE-NAME :recipe RECIPE-PLIST))
  - `update`
     Updates the menu's package candidate list.
Each function must also accept an optional ITEM as a second argument."
  ;;@TODO: document ITEM spec requirements
  :type 'hook)

(defcustom elpaca-verbosity 0 "Maximum event verbosity level shown in logs."
  :type 'integer)
(defcustom elpaca-default-remote-name "origin" "Default remote name." :type 'string)

;;@COMPAT @HACK:
;;Transient not in `package--builtin-versions' from addition in 28.1 until Emacs 30.
(when-let* (((< 27 emacs-major-version 30))
            (transient-versions '(("28.1" . (0 3 7)) ("28.2" . (0 3 7))
                                  ("29.1" . (0 4 1)) ("29.2" . (0 4 3))
                                  ("29.3" . (0 4 3)) ("29.4" . (0 4 3)))))
  (add-to-list 'package--builtin-versions
               (cons 'transient (alist-get emacs-version transient-versions
                                           nil nil #'string-prefix-p))))
(defcustom elpaca-ignored-dependencies (mapcar #'car package--builtin-versions)
  "List of IDs which are not installed unless the user explicitly requests them."
  :type '(repeat symbol))

(defvar elpaca-overriding-prompt nil "Overriding prompt for interactive functions.")

(defun elpaca--read-file (path)
  "Read file at PATH into memory."
  (when-let* (((file-exists-p path))
              (dir default-directory))
    (condition-case-unless-debug err
        (with-current-buffer (get-buffer-create " elpaca--read-file")
          (setq default-directory dir)
          (insert-file-contents path nil nil nil 'replace)
          (goto-char (point-min))
          (read (current-buffer)))
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

(cl-defstruct (elpaca-q (:constructor elpaca-q<-create)
                        (:type list) (:copier nil)
                        (:named) (:conc-name elpaca-q<-))
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

(defsubst elpaca--first (obj)
  "Return `car' of OBJ if it is a list, else OBJ."
  (if (listp obj) (car obj) obj))

(defsubst elpaca--q (e)
  "Return E's Q."
  (and e (car (last elpaca--queues (1+ (elpaca<-queue-id e))))))

(defun elpaca--completion-group-by-menu (candidate transform)
  "Return CANDIDATE menu if TRANSFORM non-nil, otherwise CANDIDATE item."
  (if-let* ((space (string-match-p " " candidate)))
      (substring candidate (if transform 0 (1+ space)) (when transform space))
    candidate))

(defcustom elpaca-description-column 20 "Minimum column for completion descriptions."
  :type 'number)

(defun elpaca--completion-affixation-fn (candidates)
  "Return `elpaca-menu-item' CANDIDATES affixation function."
  (let ((col elpaca-description-column))
    (lambda (completions)
      (setq col (max col (or (cl-loop for c in completions maximize (string-match-p " " c)) 0)))
      (mapcar
       (lambda (c) (list c ""
                         (concat (propertize " " 'display `(space :align-to ,col))
                                 (when-let* ((item (cdr (assoc-string c candidates)))
                                             (desc (plist-get (cdr item) :description)))
                                   (propertize (concat " " desc) 'face 'completions-annotations)))))
       completions))))

;;;###autoload
(defun elpaca-menu-item (&optional id interactive)
  "Return menu item matching ID in `elpaca-menu-functions'.
If ID is nil, prompt for item. If INTERACTIVE is non-nil, copy to `kill-ring'."
  (interactive (list nil t))
  (let ((item
         (if id (run-hook-with-args-until-success 'elpaca-menu-functions 'index id) ;; Depth first search
           (cl-loop
            for menu in elpaca-menu-functions
            append (cl-loop for i in (funcall menu 'index) collect
                            (cons (concat (symbol-name (car i)) " " (plist-get (cdr i) :source)) i))
            into candidates
            finally return
            (let ((choice (completing-read
                           (or elpaca-overriding-prompt "Menu Item: ")
                           (lambda (string pred action)
                             (if (eq action 'metadata)
                                 `(metadata . ((group-function . elpaca--completion-group-by-menu)
                                               (affixation-function . ,(elpaca--completion-affixation-fn candidates))))
                               (complete-with-action action candidates string pred)))
                           nil t)))
              (alist-get choice candidates nil nil #'equal))))))
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
    (run-hook-with-args 'elpaca-menu-functions 'update)))

(defsubst elpaca--nonheritable-p (obj)
  "Return t if OBJ has explicitly nil :inherit key, nil otherwise."
  (when-let* (((listp obj))
              (member (plist-member obj :inherit)))
    (not (cadr member))))

(defun elpaca--order (&optional err)
  "Prompt for order. User ERR is messaged when no order selected."
  (if-let* ((elpaca-overriding-prompt (or elpaca-overriding-prompt "Order: "))
            (item (elpaca-menu-item)))
      (cons (car item) (plist-get (cdr item) :recipe))
    (user-error (or err "No order selected"))))

;;;###autoload
(defun elpaca-recipe (&optional order interactive)
  "Return recipe computed from ORDER.
ORDER is any of the following values:
  - nil.  The order is prompted for.
  - an ID symbol, looked up in ITEMS or `elpaca-menu-functions' cache.
  - an order list of the form: \\='(ID . PROPS).
When INTERACTIVE is non-nil, `yank' the recipe to the clipboard."
  (interactive (list (elpaca--order) t))
  (let* ((order (or order (elpaca--order)))
         (id (elpaca--first order))
         (props (let ((it (cdr-safe order)))
                  (unless (zerop (logand (length it) 1))
                    (user-error "Uneven property list for order %S %s" id it))
                  it))
         (nonheritable-p (elpaca--nonheritable-p props))
         (mods (unless nonheritable-p (run-hook-with-args-until-success
                                       'elpaca-order-functions order)))
         (inherit (plist-member props :inherit))
         (item (let ((elpaca-menu-functions
                      (unless (or interactive ;; we already queried for this.
                                  (elpaca--nonheritable-p (elpaca-merge-plists mods inherit)))
                        (if-let* ((menus (cadr inherit))
                                  ((not (eq menus t))))
                            (if (listp menus) menus (list menus))
                          elpaca-menu-functions))))
                 (elpaca-menu-item id)))
         (item-recipe (plist-put (plist-get item :recipe) :source (plist-get item :source)))
         (r (elpaca-merge-plists item-recipe mods props)))
    (unless (plist-get r :package) (setq r (plist-put r :package (symbol-name id))))
    (when-let* ((recipe-mods (run-hook-with-args-until-success 'elpaca-recipe-functions r)))
      (setq r (elpaca-merge-plists r recipe-mods)))
    (when-let* ((remotes (plist-get r :remotes)) ;; Normalize :remotes to list of specs
                ((not (ignore-errors (mapc #'length remotes)))))
      (setq r (plist-put r :remotes (list remotes))))
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
  (file-name-base (substring string (- (or (string-match-p "/" (reverse string))
                                           (error "Invalid repo name %S" string))))))

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
  (let* ((url (plist-get recipe :url))
         (repo (plist-get recipe :repo))
         (remote (car-safe repo))
         (local (cdr-safe repo))
         (pkg (plist-get recipe :package))
         (host (or (plist-get recipe :host) (plist-get recipe :fetcher)))
         (hostname (and host (prin1-to-string host 'noescape)))
         (user nil)
         (info (concat url (or remote repo) hostname))
         (key (or (and info (> (length info) 0) (intern info))
                  (error "Cannot determine URL from recipe: %S" recipe)))
         (mono-repo (elpaca-alist-get key elpaca--repo-dirs))
         (dirs (and (not mono-repo) (mapcar #'cdr elpaca--repo-dirs)))
         (name (cond
                (local
                 (if-let* ((owner (assoc local dirs)))
                     (error "Local repo %S owned by %s" local (cdr owner))
                   local))
                (mono-repo (car mono-repo))
                (url
                 (unless (featurep 'url-parse) (require 'url-parse))
                 (file-name-base (directory-file-name (url-filename
                                                       (url-generic-parse-url url)))))
                (repo (if-let* ((r (or remote repo))
                                ((eq (elpaca--repo-type r) 'local)))
                          (if local
                              (file-name-base (directory-file-name local))
                            r)
                        (when host (setq user (elpaca--repo-user r)))
                        (elpaca--repo-name (or local r))))
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
    (when (consp repo) (setq repo (car repo))) ; Handle :repo rename
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

(cl-defstruct (elpaca (:constructor elpaca<--create) (:type list) (:named)
                      (:conc-name elpaca<-))
  "Order for queued processing."
  id package order statuses
  repo-dir build-dir mono-repo main
  files build-steps recipe blocking blockers
  dependencies dependents
  (queue-id (1- (length elpaca--queues)))
  (queue-time (current-time))
  (init (not after-init-time))
  process log builtp)

(defun elpaca--queued (&optional n)
  "Return list of elpacas from Nth queue.
If N is nil return a list of all queued elpacas."
  (if n (elpaca-q<-elpacas (nth n elpaca--queues))
    (cl-loop for queue in elpaca--queues append (elpaca-q<-elpacas queue))))

(defsubst elpaca--mono-repo (id repo)
  "Return previously queued E with REPO other than ID."
  (cl-loop for (_ . e) in (reverse (elpaca--queued)) thereis
           (and (not (eq (elpaca<-id e) id)) (equal repo (elpaca<-repo-dir e)) e)))

(defun elpaca--build-steps (recipe &optional builtp clonedp mono-repo)
  "Return list of build functions for RECIPE.
BUILTP, CLONEDP, and MONO-REPO control which steps are excluded."
  (when-let* ((defaults (if builtp elpaca--pre-built-steps elpaca-build-steps))
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
      (when clonedp (setq steps (remq 'elpaca--clone steps)))
      steps)))

(defsubst elpaca--status (e) "Return E's status." (car (elpaca<-statuses e)))

(defcustom elpaca-log-command-queries
  '(((elpaca-fetch elpaca-fetch-all elpaca-log-updates) . "#latest #update-log")
    ((elpaca-try elpaca-rebuild) . "#latest #linked-errors")
    (( elpaca-merge elpaca-merge-all elpaca-pull elpaca-pull-all
       elpaca-update elpaca-update-all)
     . "#latest #unique")
    ((eval-buffer eval-region eval-defun eval-last-sexp org-ctrl-c-ctrl-c) . silent)
    (elpaca-delete . (lambda () (if (derived-mode-p 'elpaca-ui-mode)
                                    elpaca-ui-search-query 'silent)))
    (elpaca-ui-execute-marks . (lambda () (require 'elpaca-log) (elpaca-log--marked-query))))
  "Alist of form ((COMMAND-OR-COMMAND-LIST . QUERY-OR-FUNCTION)...).
If query is a string it is used when logging for that command.
If it is a function, it's return value is used."
  :type 'alist :group 'elpaca-ui)

(defun elpaca--log-find-command (val key)
  "Return t if KEY VAL."
  (or (eq key val) (and (listp val) (member key val))))

(defun elpaca-log-command-query ()
  "Return logging query matching `this-command' in `elpaca-log-command-queries'."
  (when-let* ((found (alist-get this-command elpaca-log-command-queries
                                nil nil #'elpaca--log-find-command)))
    (if (functionp found) (funcall found) found)))

(defun elpaca-log-initial-queues ()
  "Return logging query if initial queues require building or order fails."
  (unless elpaca-after-init-time
    (cl-loop for (_ . e) in (elpaca--queued)
             for query = (cond ((not (elpaca<-builtp e)) "#unique | !finished")
                               ((eq (elpaca--status e) 'failed) "| failed"))
             when query return
             (prog1 query
               (setq initial-buffer-choice
                     (let ((ibc initial-buffer-choice))
                       (lambda ()
                         (add-hook 'elpaca-after-init-hook
                                   (lambda () (setq initial-buffer-choice ibc)))
                         (get-buffer-create "*elpaca-log*"))))))))

(declare-function elpaca-log-defaults "elpaca-log")
(declare-function elpaca-log-initial-queues "elpaca-log")
(defcustom elpaca-log-functions '(elpaca-log-initial-queues elpaca-log-command-query)
  "Hook run prior to logging.
It's functions should return either:
  - t to log with the buffer's default filter.
  - a string which is used as the search query.
  - `silent' to prevent logging altogether.
  - nil to skip the function.

The first function, if any, which returns non-nil is used." :type 'hook)

(defvar elpaca--log-request-time nil "Time of most recent log event.")
(declare-function elpaca-log "elpaca-log")
(defun elpaca--maybe-log ()
  "Log if `elpaca-log-functions' return non-nil."
  (when-let* ((query (run-hook-with-args-until-success 'elpaca-log-functions)))
    (require 'elpaca-log)
    (unless (eq query 'silent)
      (setq elpaca--log-request-time (current-time)) ;;@MAYBE: move into elpaca-log?
      (elpaca-log (cond ((eq query t) nil)
                        ((stringp query) query)
                        (t (signal 'wrong-type-error `((stringp t) ,query))))
                  t))))

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
         (blockers nil)
         (mono-repo (when-let* (((not builtp))
                                (e (elpaca--mono-repo id repo-dir)))
                      (when (and (eq (elpaca<-queue-id e) (elpaca-q<-id (car elpaca--queues)))
                                 (not (memq 'ref-checked-out (elpaca<-statuses e))))
                        (setq status 'blocked info (concat "Waiting on monorepo " repo-dir))
                        (push (elpaca<-id e) blockers)
                        (cl-pushnew id (elpaca<-blocking e)))
                      e))
         (build-steps (elpaca--build-steps recipe builtp clonedp mono-repo)))
    (when (memq id elpaca-ignored-dependencies)
      (setq elpaca-ignored-dependencies (delq id elpaca-ignored-dependencies)))
    (elpaca<--create
     :id id :package (symbol-name id) :order order :statuses (list status)
     :repo-dir repo-dir :build-dir build-dir :mono-repo mono-repo
     :build-steps build-steps :recipe recipe :builtp builtp :blockers blockers
     :log (list (list status nil info 0)))))

(declare-function elpaca-ui--update-search-query "elpaca-ui")
(defun elpaca--update-log-buffer ()
  "Update views in `elpaca-log-buffer'."
  (when-let* ((log (bound-and-true-p elpaca-log-buffer))
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
         (queued (and new-status-p (elpaca--queued))))
    (when-let* ((new-status-p)
                ((push status (elpaca<-statuses e)))
                ((memq status elpaca--inactive-states)))
      (setq elpaca--status-counts (elpaca--count-statuses))
      (dolist (blocked (elpaca<-blocking e))
        (elpaca--check-status e (elpaca-alist-get blocked queued)))
      (and (not elpaca-after-init-time) (eq status 'failed) (elpaca--maybe-log)))
    (when info (elpaca--log e info verbosity replace))
    (when (<= (or verbosity 0) elpaca-verbosity)
      (when elpaca--log-timer (cancel-timer elpaca--log-timer))
      (if elpaca--waiting ; Don't set timer. We're already polling.
          (elpaca--update-log-buffer)
        (setq elpaca--log-timer
              (and info (run-at-time elpaca-log-interval nil #'elpaca--update-log-buffer))))))
  nil)

;;@TODO: Should this cause a non-local exit? Signal custom error type or just `throw'?
;;@MAYBE: UI command to manually fail an order?
(defun elpaca--fail (e &optional reason)
  "Fail E for REASON."
  (unless (eq (elpaca--status e) 'failed)
    (let ((q (elpaca--q e)))
      (setf (elpaca-q<-forms q) (assq-delete-all (elpaca<-id e) (elpaca-q<-forms q))))
    (when-let* ((p (elpaca<-process e)))
      (when-let* ((entry (car (last (elpaca<-log e) (process-get p :loglen)))))
        (setf (nth 2 entry) (propertize (nth 2 entry) 'face 'elpaca-failed)))
      (when (process-live-p p) (kill-process p)))
    (push 'reclone (elpaca<-statuses e)) ;;@HACK: Prevent reclone when legitimate failure during clone.
    (elpaca--signal e reason 'failed)
    (elpaca--finalize e)))

(defun elpaca-get (id)
  "Return queued E associated with ID."
  (elpaca-alist-get id (elpaca--queued)))

(defun elpaca--run-build-commands (commands)
  "Run build COMMANDS."
  (dolist (command (if (listp (car-safe commands)) commands (list commands)))
    (message "Running command: %S" command)
    (if (cl-every #'stringp command)
        (apply #'elpaca-process-poll command)
      (eval command t))))

(defun elpaca--caller-name (n &rest skip)
  "Return Nth calling function's name, skipping symbols in SKIP."
  (cl-loop with current = (backtrace-frame (1+ n)) ;; Skip this function call.
           with previous = nil
           while (or (null (car current)) (memq (cadr current) skip))
           do (setq previous current current (backtrace-frame (cl-incf n)))
           finally return (cadr (or current previous))))

(defun elpaca--continue-build (e &rest args)
  "Run E's next build step.
Optional ARGS are passed to `elpaca--signal', which see."
  (elpaca--signal e (format "Continued by: %S"
                            (elpaca--caller-name -1 'backtrace-frame 'elpaca--process-sentinel 'apply
                                                 'elpaca--caller-name 'elpaca--continue-build))
                  nil nil 2)
  (when args (apply #'elpaca--signal e args))
  (unless (memq (elpaca--status e) elpaca--inactive-states)
    (if-let* ((elpaca-queue-limit)
              ((not (elpaca<-builtp e)))
              ;; Don't double count current E
              (active (1- (cl-loop for (_ . e) in (elpaca-q<-elpacas (elpaca--q e))
                                   count (not (memq (elpaca--status e) elpaca--inactive-states)))))
              ((>= active elpaca-queue-limit)))
        (unless (eq (elpaca--status e) 'blocked) ;;@MAYBE: check for queue-throttled, too?
          (push 'queue-throttled (elpaca<-statuses e))
          (elpaca--signal e "elpaca-queue-limit exceeded" 'blocked nil 1))
      (let ((step (or (pop (elpaca<-build-steps e)) #'elpaca--finalize)))
        (condition-case-unless-debug err ;;@TODO: signal/catch custom error types
            (if-let* ((vars (plist-get (elpaca<-recipe e) :vars))
                      (fn `(lambda (elpaca elpaca-build-step)
                             (let* (,@vars) (funcall elpaca-build-step elpaca)))))
                (funcall fn e step)
              (funcall step e))
          ((error) (elpaca--fail e (format "%s: %S" step err))))))))

(defun elpaca--log-duration (e)
  "Return E's log duration."
  (time-subtract (nth 1 (car (elpaca<-log e))) (elpaca<-queue-time e)))

(defun elpaca--queue (order &optional queue)
  "ADD ORDER to QUEUE or current queue.  Return E."
  (if-let* ((id (elpaca--first (or order (signal 'wrong-type-argument
                                                 '((or symbolp listp) nil)))))
            ((not after-init-time))
            (e (elpaca-get id)))
      (if-let* ((dependents (elpaca<-dependents e)))
          (warn "%S previously queued as dependency of: %S" id dependents)
        (warn "Duplicate item ID queued: %S" id))
    (let* ((e (elpaca<-create order))
           (log (pop (elpaca<-log e)))
           (status (car log))
           (info (nth 2 log))
           (q (or queue (car elpaca--queues))))
      (when queue (setf (elpaca<-queue-id e) (elpaca-q<-id q)
                        (elpaca<-init e) (elpaca-q<-type q)))
      (setf (alist-get id (elpaca-q<-elpacas q)) e)
      (if (eq status 'struct-failed)
          (elpaca--fail e info)
        (elpaca--signal e info status nil 1))
      e)))

;;;###autoload
(defun elpaca-split-queue (&rest args)
  "Split remaining elpacas into new queue with ARGS."
  (unless (elpaca-q<-elpacas (car elpaca--queues)) (pop elpaca--queues))
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
  (when-let* ((autoloads (elpaca-q<-autoloads q)))
    (condition-case-unless-debug err
        (eval `(progn ,@(reverse autoloads)) t)
      ((error) (warn "Autoload Error: %S" err))))
  (setf (elpaca-q<-status q) 'complete) ; Avoid loop when forms call elpaca-process-queue.
  (when-let* ((forms (nreverse (elpaca-q<-forms q))))
    (with-current-buffer (get-buffer-create " *elpaca--finalize-queue*")
      (setq-local lexical-binding t)
      (cl-loop for (id . body) in forms
               do (condition-case-unless-debug err
                      (eval `(progn ,@body) t)
                    ((error) (warn "Config Error %s: %S" id err)))))
    (setf (elpaca-q<-forms q) nil))
  (run-hooks 'elpaca-post-queue-hook)
  (let ((next (nth (1+ (elpaca-q<-id q)) (reverse elpaca--queues))))
    (unless (or elpaca-after-init-time ; Already run.
                elpaca--waiting ; Won't know if final queue until after waiting.
                (not (eq (elpaca-q<-type q) 'init)) ; Already run.
                (and next (eq (elpaca-q<-type next) 'init))) ; More init queues.
      (elpaca-split-queue)
      (setq elpaca-after-init-time (current-time))
      (run-hooks 'elpaca-after-init-hook))
    (if (and next (elpaca-q<-elpacas next))
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
         (es (elpaca-q<-elpacas q)))
    (when-let* ((elpaca-queue-limit)
                (next (cl-loop for (_ . e) in es thereis (and (elpaca--throttled-p e) e))))
      (elpaca--signal next nil 'unthrottled)
      (elpaca--continue-build next))
    (when (= (cl-incf (elpaca-q<-processed q)) (length es)) (elpaca--finalize-queue q))))

(defun elpaca--propertize-subprocess (process)
  "Propertize PROCESS according to exit status in associated E."
  (when-let* ((e (process-get process :elpaca))
              (entry (car (last (elpaca<-log e) (process-get process :loglen)))))
    (setf (nth 2 entry) (propertize (nth 2 entry) 'face
                                    (if (zerop (process-exit-status process))
                                        'elpaca-finished 'elpaca-failed)))))

(defun elpaca--command-string (strings &optional prefix)
  "Return string of form PREFIX STRINGS."
  (concat (or prefix "$") (string-join strings " ")))

(defun elpaca--call-with-log (e verbosity &rest command)
  "Call and Log E's COMMAND and output with VERBOSITY."
  (elpaca-with-process (apply #'elpaca-process-call command)
    (elpaca--signal e (propertize (elpaca--command-string command)
                                  'face (if success 'elpaca-finished 'elpaca-failed))
                    nil nil verbosity)
    (when-let* ((output (string-trim (concat stdout stderr)))
                ((not (string-empty-p output))))
      (elpaca--signal e output nil nil verbosity))
    result))

(defun elpaca--initial-fetch (e)
  "Perform initial fetch for E, respecting :remotes recipe inheritance."
  (let* ((recipe (copy-tree (elpaca<-recipe e)))
         (remotes (plist-get recipe :remotes)))
    (setf recipe (elpaca-merge-plists recipe '(:remotes nil)))
    (cl-loop for remote in remotes
             for opts = (elpaca-merge-plists recipe (cdr-safe remote))
             for command = `("git" "fetch" ,@(when-let* ((depth (plist-get opts :depth))
                                                         ((numberp depth)))
                                               (list "--depth" (format "%s" depth)))
                             ,(or (car-safe remote) remote))
             for fn = `(lambda (e) (elpaca--fetch e ,@command))
             do (push fn (elpaca<-build-steps e))
             finally (elpaca--continue-build e))))

(defun elpaca--configure-remotes (e)
  "Add and/or rename E's repo remotes."
  (let ((fetchp nil))
    (when-let* ((default-directory (elpaca<-repo-dir e))
                (recipe            (elpaca<-recipe   e))
                (remotes           (plist-get recipe :remotes)))
      (elpaca--signal e "Configuring Remotes" 'adding-remotes)
      (cl-loop with renamed for spec in remotes do
               (if (stringp spec)
                   (if renamed
                       (elpaca--signal e (format "ignoring :remotes rename %S" spec))
                     (unless (equal spec elpaca-default-remote-name)
                       (elpaca--call-with-log
                        e 1 "git" "remote" "rename" elpaca-default-remote-name spec))
                     (setq renamed spec))
                 (when-let* ((remote    (car spec))
                             (props     (cdr spec))
                             (inherited (elpaca-merge-plists recipe props))
                             (URI       (elpaca--repo-uri inherited)))
                   (setq fetchp t)
                   (elpaca-with-process
                       (elpaca--call-with-log e 1 "git" "remote" "add" remote URI)
                     (unless success (elpaca--fail e stderr)))))))
    (when fetchp (push #'elpaca--initial-fetch (elpaca<-build-steps e))))
  (elpaca--continue-build e))

(defun elpaca--remove-build-steps (e spec)
  "Remove each step in SPEC from E."
  (setf (elpaca<-build-steps e) (cl-set-difference (elpaca<-build-steps e) spec)))

(defun elpaca--files (e &optional files nocons)
  "Return alist of E :files to be symlinked: (PATH . TARGET PATH).
FILES and NOCONS are used recursively."
  (cl-loop
   with repo-dir = (elpaca<-repo-dir e)
   with default-directory = repo-dir
   with build-dir = (elpaca<-build-dir e)
   with recipe    = (elpaca<-recipe e)
   with files     = (or files (if-let* ((member (plist-member recipe :files)))
                                  (cadr member) elpaca-default-files-directive))
   with (exclusions targets with-subdirs)
   for el in files do
   (pcase el
     ((pred stringp) (push (or (file-expand-wildcards el) el) targets))
     (`(:exclude  . ,excluded) (push (elpaca--files e excluded 'nocons) exclusions))
     (:defaults (push (elpaca--files e elpaca-default-files-directive 'nocons) targets))
     ;;@FIX: subdir needn't be same name as globbed path...
     (`(,_subdir . ,paths)
      (cl-loop for path in paths
               for expanded = (file-expand-wildcards path)
               do (cl-loop for path in expanded
                           do (push (cons (expand-file-name path repo-dir)
                                          (expand-file-name path build-dir))
                                    with-subdirs)))))
   finally return
   (let ((targets (cl-nset-difference (flatten-tree targets) (flatten-tree exclusions)
                                      :test #'equal)))
     (if nocons
         targets
       (append with-subdirs
               (cl-loop for target in targets when (file-exists-p target)
                        collect (cons (expand-file-name target)
                                      (expand-file-name (file-name-nondirectory target)
                                                        build-dir))))))))

(defun elpaca--find-source-file-maybe ()
  "Find corresponding source file for `current-buffer'."
  (when-let* ((file (buffer-file-name))
              ((string-prefix-p elpaca-builds-directory file))
              (e (cdr (cl-find-if (lambda (bdir) (string-match-p bdir file))
                                  (elpaca--queued)
                                  :key (lambda (qd) (elpaca<-build-dir (cdr qd))))))
              (source (car (rassoc file (elpaca--files e))))
              ((file-exists-p source)))
    (find-alternate-file source)
    (message "Found Elpaca build file source")))

;;;###autoload
(define-minor-mode elpaca-no-symlink-mode
  "Global minor mode which installs build files by copying."
  :global t :group 'elpaca
  (if elpaca-no-symlink-mode
      (add-hook 'find-file-hook #'elpaca--find-source-file-maybe)
    (remove-hook 'find-file-hook #'elpaca--find-source-file-maybe)))

(defun elpaca--link-build-files (e)
  "Link E's :files into its builds subdirectory."
  (elpaca--signal e "Linking build files" 'linking)
  (let* ((build-dir (elpaca<-build-dir e))
         (files (or (elpaca<-files e)
                    (setf (elpaca<-files e) (elpaca--files e)))))
    (when (file-exists-p build-dir) (delete-directory build-dir 'recusrive))
    (make-directory build-dir 'parents)
    (dolist (spec files)
      (when-let* ((file   (car spec))
                  ((file-exists-p file))
                  (link   (cdr spec)))
        (make-directory (file-name-directory link) 'parents)
        (if elpaca-no-symlink-mode
            (if (file-directory-p file)
                (copy-directory file link nil 'parents 'recursive)
              (copy-file file link 'overwrite))
          (make-symbolic-link file link 'overwrite)))))
  (setf (elpaca<-builtp e) t)
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

(defvar elpaca--eol (if (memq system-type '(ms-dos windows-nt cygwin)) "\r\n" "\n"))
(defun elpaca--process-filter (process output)
  "Filter PROCESS OUTPUT."
  (process-put process :raw-output (concat (process-get process :raw-output) output))
  (let* ((e       (process-get process :elpaca))
         (parsed  (process-get process :parsed))
         (timer   (process-get process :timer))
         (chunk   (concat parsed output))
         (lines   (split-string chunk elpaca--eol))
         (linep   (string-empty-p (car (last lines)))))
    (when timer (cancel-timer timer))
    (unless (eq (elpaca--status e) 'failed)
      (process-put
       process :timer (run-at-time elpaca-busy-interval nil #'elpaca--update-log-buffer)))
    (unless linep
      (process-put process :parsed (car (last lines)))
      (setq lines (butlast lines)))
    (dolist (line lines)
      (unless (string-empty-p line)
        (elpaca--signal e (concat "  " (car (last (split-string line "\r" t)))))))))

(defun elpaca--process-sentinel (&optional info status process event)
  "Update E's INFO and STATUS when PROCESS EVENT is finished."
  (if-let* ((e (process-get process :elpaca))
            ((and (equal event "finished\n") (not (eq (elpaca--status e) 'failed)))))
      (progn (elpaca--propertize-subprocess process)
             (elpaca--continue-build e info status))
    (elpaca--fail e (format "Subprocess error (see previous log entries)"))))

(defun elpaca--compile-info-process-sentinel (process event)
  "Sentinel for info compilation PROCESS EVENT."
  (let* ((e  (process-get process :elpaca))
         (finished (equal event "finished\n")))
    (unless finished
      (setf (elpaca<-build-steps e)
            (cl-set-difference (elpaca<-build-steps e) '(elpaca--install-info elpaca--add-info-path))))
    (elpaca--propertize-subprocess process)
    (elpaca--continue-build
     e (if finished "Info compiled" (concat "Compilation failure: " (string-trim event))))))

(defun elpaca--make-process (e &rest spec)
  "Attach process to E from `make-process' SPEC plist."
  (declare (indent 1))
  (let* ((command (plist-get spec :command))
         (_ (elpaca--signal ;;@HACK Signal before process started. SIGSTP/SIGSTOP failed orders.
             e (propertize (elpaca--command-string command) 'face 'elpaca-blocked)))
         (process (make-process ;;@FIX: Find way to stop process and continue after bookkeeping
                   :name (concat "elpaca-" (plist-get spec :name) "-" (elpaca<-package e))
                   :connection-type (or (plist-get spec :connection-type) 'pipe)
                   :command command
                   :filter (or (plist-get spec :filter) #'elpaca--process-filter)
                   :sentinel (plist-get spec :sentinel))))
    (process-put process :elpaca e)
    (process-put process :loglen (length (elpaca<-log e)))
    (setf (elpaca<-process e) process)))

(defcustom elpaca-with-emacs-env-form
  '(setq gc-cons-percentage 1.0 print-level nil print-circle nil)
  "Form evaluated by `elpaca-with-emacs' subprocesses.
Used to modify the subprocess environment."
  :type 'sexp)

;;;###autoload
(defmacro elpaca-with-emacs (e &optional args &rest forms)
  "Execute E's FORMS in an async Emacs subprocess with ARGS.
ARGS must be a plist including any of the following keywords value pairs:

:name an expression evaluating to a string, used as the subprocess name.
:args an expression evaluating to a list of Emacs subprocess command line args.
:env a `let' VARLIST which is evaluated and injected in the subprocess."
  (declare (indent 1) (debug t))
  (let ((esym (make-symbol "e"))
        (formsym (make-symbol "forms"))
        (argsym (make-symbol "args"))
        (namesym (make-symbol "name")))
    `(let* ((,esym ,e)
            (,formsym (backquote ,forms))
            (,argsym (backquote ,args))
            (,namesym (or (plist-get ,argsym :name)
                          (concat "elpaca-with-emacs-" (elpaca<-package ,esym)))))
       (unless (keywordp (car-safe ,argsym))
         (setq ,formsym (append (list ,argsym) ,formsym) ,argsym nil))
       (elpaca--make-process ,esym
         :name ,namesym
         :buffer ,namesym
         :command (list (elpaca--emacs-path) "-Q" "--batch" ,@(plist-get args :args)
                        "--eval" (format "%S" elpaca-with-emacs-env-form)
                        "--eval" (format "%S" (macroexp-progn ,formsym)))
         :sentinel
         (lambda (process event)
           (elpaca--process-sentinel (concat ,namesym " complete") nil process event))))))

(defun elpaca--compile-info (e)
  "Compile E's .texi files."
  (elpaca--signal e "Compiling Info files" 'info)
  (if-let* ((default-directory (elpaca<-build-dir e))
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
    (elpaca--propertize-subprocess process)
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
  (when-let* ((elpaca-install-info-executable)
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
                      (push `(lambda (e) (elpaca--install-info-async ,file ,dir e))
                            (elpaca<-build-steps e)))))
  (elpaca--continue-build e (unless elpaca-install-info-executable
                              "No elpaca-install-info-executable")))

(defun elpaca--dispatch-build-commands-process-sentinel (process event)
  "PROCESS EVENT."
  (let ((e    (process-get process :elpaca))
        (type (process-get process :build-type)))
    (elpaca--propertize-subprocess process)
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
  (if-let* ((recipe   (elpaca<-recipe e))
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
;;covers all sorts of general edge cases. e.g. tramp remote files.
(defun elpaca--directory-files-recursively (directory regexp)
  "Return DIRECTORY files matching REGEXP."
  (let ((default-directory (expand-file-name directory)))
    (flatten-tree
     (cl-loop for file in (directory-files ".")
              unless (string-match-p "\\(?:\\`\\.\\)" file)
              collect (if (file-directory-p file)
                          (unless (file-symlink-p file)
                            (elpaca--directory-files-recursively file regexp))
                        (when (string-match-p regexp file) (expand-file-name file)))))))

(defun elpaca--main-file (e &optional recache)
  "Return E's main file name. Recompute when RECACHE non-nil."
  (or (and (not recache) (elpaca<-main e))
      (setf (elpaca<-main e)
            (if-let* ((recipe (elpaca<-recipe e))
                      (declared (plist-member recipe :main)))
                (cadr declared)
              (let* ((repo (elpaca<-repo-dir e))
                     (default-directory repo)
                     (package (file-name-sans-extension (elpaca<-package e)))
                     (name (concat "\\(?:" (regexp-quote package) "\\(?:-pkg\\)?\\.el\\)\\'")))
                (or (file-exists-p repo) (error "Repository not on disk"))
                (or

                 (car (directory-files repo nil (concat "\\`" name)))
                 (car (cl-remove-if-not
                       (lambda (f) (string-match-p (concat "[/\\]" name) f))
                       (elpaca--directory-files-recursively repo (concat "\\`[^.z-a]*" name))))
                 (error "Unable to find main elisp file for %S" package)))))))

(defvar elpaca--tag-regexp
  "\\`\\(?:\\|[RVrv]\\|release[/-]v?\\)?\\(?1:[0-9]+\\(\\.[0-9]+\\)*\\)\\'")
(defun elpaca-latest-tag (e)
  "Return E's latest merged tag matching recipe tag regexp or `elpaca--tag-regexp'."
  (when-let* ((default-directory (elpaca<-repo-dir e))
              (recipe (elpaca<-recipe e))
              (regexp (or (plist-get recipe :version-regexp) elpaca--tag-regexp))
              (tags (elpaca-with-process
                        (elpaca-process-call "git" "tag" "--sort=-creatordate" "--merged")
                      (and success stdout (split-string stdout "\n" 'omit-nulls)))))
    (cl-loop for tag in tags when (string-match regexp tag)
             return (or (match-string 1 tag) (match-string 0 tag)))))

(defun elpaca--commit-date (e &optional format)
  "Return date of E's checked out commit with FORMAT spec."
  (let ((default-directory (elpaca<-repo-dir e)))
    (elpaca--with-no-git-config
     (elpaca-with-process-call ("git" "log" "-n" "1" "--format=%cd" "--date=unix")
       (if (not success) (elpaca--fail e stderr)
         (format-time-string (or format "%Y%m%d.%H%M") (seconds-to-time (string-to-number stdout))
                             0))))))

(defun elpaca--parse-version (file)
  "Parse FILE's version via package header or pkg file data."
  (setq file (expand-file-name file))
  (with-current-buffer (get-buffer-create " *elpaca--dependencies*")
    (setq default-directory (file-name-directory file))
    (insert-file-contents-literally file nil nil nil 'replace)
    (goto-char (point-min))
    (if (string-suffix-p "-pkg.el" file)
        (nth 2 (read (current-buffer)))
      (when-let* ((case-fold-search t)
                  (regexp "^;+[ ]+\\(Package-\\)?\\(Version\\)[ ]*:[ ]*")
                  ((re-search-forward regexp nil 'noerror)))
        ;; Remove Windows \r before newline. See: #218
        (string-trim (buffer-substring-no-properties (point) (line-end-position)))))))

(defun elpaca--declared-version (e)
  "Return E's version as declared in recipe or main file's metadata."
  (if-let* ((declared (plist-get (elpaca<-recipe e) :version)))
      (funcall declared e)
    (when-let* ((main (elpaca--main-file e))
                (default-directory (elpaca<-repo-dir e)))
      (elpaca--parse-version main))))

(defconst elpaca--emacs-releases '(("27.1" . 20200804) ("27.2" . 20210319) ("28.1" . 20220403)
                                   ("28.2" . 20220912) ("29.1" . 20230730) ("29.2" . 20240118)
                                   ("29.3" . 20240324) ("29.4" . 20240622) ("30.1" . 20250223)
                                   ("30.2" . 20250814)))
(defvar elpaca-core-date
  (let ((release (assoc emacs-version elpaca--emacs-releases #'string-prefix-p)))
    (and release (> (length (version-to-list emacs-version)) 2) ;; Development version.
         (lwarn `(elpaca core stale ,(intern emacs-version)) :warning
                "Emacs %s assigned %s elpaca-core-date." emacs-version (car release)))
    (list (or (cdr release)
              (and emacs-build-time (string-to-number (format-time-string "%Y%m%d" emacs-build-time)))
              (and (display-warning `(elpaca core ,(intern emacs-version))
                                    "Unable to determine elpaca-core-date")
                   -1))))
  "List of form (N) where N is a YYYYMMDD integer release date of Emacs.")
(defconst elpaca--date-version-schema-min 10000000)

(defun elpaca--check-version (e)
  "Ensure E's dependency versions are met."
  (cl-loop
   initially (elpaca--signal e "Checking dependency versions")
   with queued = (elpaca--queued)
   with version-regexp-alist = ;; Handle -dev, -DEV, etc. Why isn't this default?
   (append version-regexp-alist '(("\\(?:-[[:alpha:]]+\\)" . -1)))
   for (id need) in (elpaca--dependencies e)
   for min = (version-to-list need)
   for datep = (> (car min) elpaca--date-version-schema-min) ;; YYYYMMDD version.
   for dep = (elpaca-alist-get id queued)
   for core = (unless dep (elpaca-alist-get id package--builtin-versions))
   for version = (cond (core (if datep elpaca-core-date core))
                       ((memq id elpaca-ignored-dependencies) 'skip)
                       ((null dep) (cl-return (elpaca--fail e (format "Missing dependency %s" id))))
                       (datep (version-to-list (elpaca--commit-date dep)))
                       (t (version-to-list (or (elpaca--declared-version dep) "0"))))
   unless (eq version 'skip)
   when (or (and core (version-list-< version min))
            (and (version-list-< version min)
                 (if-let* ((tag (elpaca-latest-tag dep))
                           ((setq version (version-to-list tag))))
                     (version-list-< version min)
                   (null tag))))
   do (cl-return (elpaca--fail e (format "%s installed version %s lower than min required %s"
                                         id version need))))
  (elpaca--continue-build e))

(defun elpaca--dependencies (e &optional recache)
  "Return a list of E's declared dependencies.
If RECACHE is non-nil, do not use cached dependencies."
  (let ((cache (elpaca<-dependencies e)))
    (if-let* (((or recache (not cache)))
              (repo (elpaca<-repo-dir e))
              (package (file-name-sans-extension (elpaca<-package e)))
              (main (elpaca--main-file e)))
        (let ((deps
               (condition-case err
                   (with-current-buffer (get-buffer-create " *elpaca--dependencies*")
                     (setq default-directory repo) ;; Necessary due to recycled buffer.
                     (insert-file-contents-literally main nil nil nil 'replace)
                     (goto-char (point-min))
                     (if (string-suffix-p "-pkg.el" main) (eval (nth 4 (read (current-buffer))))
                       (when-let*
                           ((case-fold-search t)
                            ((re-search-forward
                              "^;+[ ]+\\(Package-Requires\\)[ ]*:[ ]*" nil 'noerror))
                            (deps (list (buffer-substring-no-properties (point) (line-end-position)))))
                         (forward-line 1)
                         (while (looking-at "^;+\\(\t\\|[\t\s]\\{2,\\}\\)\\(.+\\)")
                           (push (match-string-no-properties 2) deps)
                           (forward-line 1))
                         (mapcar (lambda (d) (if (cdr-safe d) d (list (elpaca--first d) "0")))
                                 (read (string-join (nreverse deps) " "))))))
                 ((end-of-file) (warn "Malformed dependency metadata: %S" main) nil)
                 ((error) (error "Dependency detection error: %S %S" main err)))))
          (setf (elpaca<-dependencies e) (or deps :nil))
          deps)
      (and (not (eq cache :nil)) cache))))

(defun elpaca--continue-dependency (e)
  "Continue E unless it has already been continued."
  (unless (memq 'continued-dep (elpaca<-statuses e))
    (elpaca--continue-build e nil 'continued-dep)))

(defun elpaca--queue-dependencies (e)
  "Queue E's dependencies."
  (cl-loop
   initially (elpaca--signal e "Queueing Dependencies" 'blocked nil 1)
   with externals = (or (cl-loop for (id . _) in (elpaca--dependencies e)
                                 unless (memq id elpaca-ignored-dependencies) collect id)
                        (cl-return (elpaca--continue-build e "No external dependencies" 'unblocked)))
   with q = (elpaca--q e)
   with qd = (elpaca--queued)
   with finished = 0
   with q-id = (elpaca<-queue-id e)
   with e-id = (elpaca<-id e)
   with builtp = (elpaca<-builtp e)
   with pending
   for dependency in externals
   for queued = (elpaca-alist-get dependency qd)
   for d = (or queued (elpaca--queue dependency q))
   for d-id = (elpaca<-id d)
   for d-status = (elpaca--status d)
   do (and queued (> (elpaca<-queue-id d) q-id)
           (cl-return (elpaca--fail d (format "dependent %S in past queue" e-id))))
   (cl-pushnew e-id (elpaca<-dependents d))
   (and builtp (not (elpaca<-builtp d)) (cl-pushnew #'elpaca--check-version (elpaca<-build-steps e)))
   (when (or (eq d-status 'queued)
             (and (elpaca--throttled-p d) (= elpaca-queue-limit 1) ;; Dependency must be continued.
                  (progn (setf (elpaca<-statuses d) (delq 'continued-dep (elpaca<-statuses d)))
                         (elpaca--signal d nil 'unthrottled)
                         t))
             (and (memq e-id (elpaca<-blockers d)) ;; Mono-repo dep blocked.
                  (equal (elpaca<-repo-dir e) (elpaca<-repo-dir d))
                  (progn (setf (elpaca<-blockers d) (delq e-id (elpaca<-blockers d)))
                         t)))
     (push d pending))
   (if (eq d-status 'finished)
       (cl-incf finished)
     (cl-pushnew e-id (elpaca<-blocking d))
     (cl-pushnew d-id (elpaca<-blockers e)))
   finally do (if (= (length externals) finished)
                  (elpaca--continue-build e nil 'unblocked)
                (mapc #'elpaca--continue-dependency pending))))

(defun elpaca--remote-default-branch (remote)
  "Return REMOTE's \"default\" branch.
This is the branch that would be checked out upon cloning."
  (elpaca-process-cond ("git" "remote" "show" remote)
    ((and success (string-match "\\(?:[^z-a]*HEAD branch:[[:space:]]+\\([^z-a]*?\\)$\\)"
                                stdout))
     (match-string 1 stdout))
    (invoked (error "Remote default branch error: %S" stderr))
    (t (error "Unable to determine remote default branch: %S" result))))

(defun elpaca--checkout-ref (e)
  "Check out E's ref."
  (let* ((recipe (elpaca<-recipe e))
         (default-directory (elpaca<-repo-dir e))
         (remotes (plist-get recipe :remotes))
         (remote (let ((default (car remotes)))
                   (when (listp default) (setq recipe (elpaca-merge-plists recipe (cdr default))))
                   default))
         (ref    (plist-get recipe :ref))
         (tag    (plist-get recipe :tag))
         (branch (plist-get recipe :branch))
         (target (or ref tag branch)))
    (when-let* ((name    (car-safe remote))
                (default (elpaca-process-output "git" "rev-parse" "--abbrev-ref" "HEAD")))
      (elpaca--call-with-log e 1 "git" "checkout" "--detach")
      (elpaca--call-with-log e 1 "git" "branch"   "--delete" (string-trim default))
      (elpaca--call-with-log e 1 "git" "config"   "checkout.defaultRemote" name)
      (when-let* (((not branch))
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
               (branch (list "checkout" "-B" branch ; "--no-guess"?
                             (concat (or (elpaca--first remote)
                                         elpaca-default-remote-name)
                                     "/" branch)))))
          :sentinel (lambda (process event)
                      (elpaca--process-sentinel (concat target " checked out") 'ref-checked-out process event)))))))

(defun elpaca--check-status (dependency e)
  "Possibly change E's status depending on DEPENDENCY statuses."
  (cl-loop
   initially (when (eq (elpaca--status e) 'failed) (cl-return))
   with failed
   with blockers = (elpaca<-blockers e)
   with queued = (elpaca--queued)
   for blocker in blockers
   for b = (elpaca-alist-get blocker queued)
   for status = (elpaca--status b) do
   (cond ((eq status 'finished) (setf (elpaca<-blockers e) (delq blocker blockers)))
         ((eq status 'failed)   (push blocker failed)))
   finally (let ((blockers (elpaca<-blockers e)))
             (cond (failed (elpaca--fail e (format "Failed dependencies: %s" failed)))
                   (blockers (elpaca--signal e (format "Blocked by: %s" blockers)) 'blocked)
                   ((eq (elpaca--status e) 'blocked)
                    (elpaca--continue-build e (concat "Unblocked by: " (elpaca<-package dependency))
                                            'unblocked))))))

(defun elpaca--clone-process-sentinel (process _event)
  "Sentinel for clone PROCESS."
  (if-let* ((e (process-get process :elpaca))
            (success (= (process-exit-status process) 0)))
      (progn (elpaca--propertize-subprocess process)
             (elpaca--continue-build e))
    (if (or (memq 'reclone (elpaca<-statuses e))
            (not (plist-get (elpaca<-recipe e) :depth)))
        (elpaca--fail e (nth 2 (car (elpaca<-log e))))
      (setf (elpaca<-recipe e) (plist-put (elpaca<-recipe e) :depth nil))
      (elpaca--signal e "Re-cloning with recipe :depth nil" 'reclone)
      (push #'elpaca--clone (elpaca<-build-steps e))
      (elpaca--continue-build e))))

(defun elpaca--clone (e)
  "Clone E's repo to `elpaca-directory'."
  (elpaca--signal e "Cloning" 'cloning)
  (let* ((recipe  (elpaca<-recipe   e))
         (remotes (plist-get recipe :remotes))
         (remote  (and remotes (car remotes)))
         (repodir (elpaca<-repo-dir e))
         (URI     (elpaca--repo-uri recipe))
         (default-directory elpaca-directory)
         (command
          `("git" "clone"
            ;;@TODO: Some refs will need a full clone or specific branch.
            ,@(when-let* ((depth (plist-get recipe :depth)))
                (cond
                 ((plist-get recipe :ref) (elpaca--signal e "Ignoring :depth in favor of :ref"))
                 ((numberp depth) `("--depth" ,(number-to-string depth)))
                 ((memq depth '(treeless blobless))
                  (cond ((consp remote)
                         (setf (elpaca<-recipe e) (plist-put recipe :depth nil))
                         (elpaca--signal e ":remotes incompatible with treeless, blobless clones; using :depth nil"))
                        ((eq depth 'treeless) '("--filter=tree:0"))
                        ((eq depth 'blobless) '("--filter=blob:none"))))))
            ;;@FIX: allow override
            ,@(when-let* ((ref (or (plist-get recipe :branch) (plist-get recipe :tag))))
                `("--single-branch" "--branch" ,ref))
            ,@(unless (or (null remote) (stringp remote)) '("--no-checkout"))
            ,URI ,repodir)))
    (elpaca--make-process e
      :name "clone" :command command :connection-type 'pty
      :sentinel #'elpaca--clone-process-sentinel)))

(defun elpaca-generate-autoloads (package dir)
  "Generate autoloads in DIR for PACKAGE."
  (or (require 'loaddefs-gen nil t) (require 'autoload))
  (let* ((default-directory dir)
         (output (expand-file-name (or generated-autoload-file
                                       (concat package "-autoloads.el")
                                       dir)))
         (generated-autoload-file output)
         (autoload-timestamps nil)
         (backup-inhibited t)
         (version-control 'never)
         (find-file-hook nil) ; Don't clobber recentf.
         (write-file-functions nil)
         (left-margin 0)) ; Prevent spurious parens in autoloads.
    (cond
     ((fboundp 'loaddefs-generate) ;; Emacs 29+
      (cl-loop with seen
               for file in (elpaca--directory-files-recursively dir "\\.el\\'")
               for d = (file-name-directory file)
               unless (member d seen) collect d into dirs do (push d seen)
               finally do (if (null dirs) (error "No elisp files in %s" dir)
                            (loaddefs-generate dirs output nil nil nil t))))
     ((fboundp 'make-directory-autoloads) (make-directory-autoloads dir output))
     ((fboundp 'update-directory-autoloads) ;; Compatibility for Emacs < 28.1
      (with-no-warnings (update-directory-autoloads dir))))
    (when-let* ((buf (find-buffer-visiting output))) (kill-buffer buf))
    output))

(defun elpaca--generate-autoloads-async (e)
  "Generate E's autoloads asynchronously."
  (if-let* ((recipe (elpaca<-recipe e))
            (autoloads (let ((member (plist-member recipe :autoloads)))
                         (if member (cadr member) t)))
            (default-directory (elpaca<-build-dir e))
            (elpaca (expand-file-name "elpaca/" elpaca-repos-directory)))
      (progn
        (elpaca--signal e (concat "Generating autoloads: " default-directory) 'autoloads)
        (elpaca-with-emacs e
          (:name "autoloads" :args ("-L" elpaca "-l" (expand-file-name "elpaca.el" elpaca)))
          ,@(when (stringp autoloads) `((setq generated-autoload-file ,autoloads)))
          (elpaca-generate-autoloads ,(elpaca<-package e) ,default-directory)))
    (elpaca--continue-build e)))

(defun elpaca--activate-package (e)
  "Activate E's package.
Adds package's build dir to `load-path'.
Loads or caches autoloads."
  (let ((default-directory (elpaca<-build-dir e)))
    (elpaca--signal e "Activating package" 'activation)
    (when (and (not elpaca-after-init-time) (featurep (elpaca<-id e)))
      (warn "%S loaded before Elpaca activation" (elpaca<-id e)))
    (cl-pushnew default-directory load-path :test #'equal)
    (elpaca--signal e "Package build dir added to load-path")
    (if-let* ((recipe (elpaca<-recipe e))
              (key (let ((member (plist-member recipe :autoloads)))
                     (if (not member) 'undeclared (cadr member))))
              (pkg (elpaca<-package e)) ;;@MAYBE: strip file extension
              (autoloads (expand-file-name
                          (or (and (stringp key) key) (concat pkg "-autoloads.el"))))
              ((file-exists-p autoloads)))
        (if elpaca-cache-autoloads
            (let ((forms nil))
              (elpaca--signal e "Caching autoloads")
              (with-current-buffer (get-buffer-create " *elpaca--activate-package*")
                (insert-file-contents autoloads nil nil nil 'replace)
                (goto-char (point-min))
                (condition-case _
                    (while t (push (read (current-buffer)) forms))
                  ((end-of-file)))
                (push `(let ((load-file-name ,autoloads)
                             (load-in-progress t)
                             (current-load-list nil))
                         (condition-case err
                             (progn ,@(nreverse forms))
                           ((error) (warn "Error loading %S autoloads: %S" ,pkg err))))
                      (elpaca-q<-autoloads (elpaca--q e))))
              (elpaca--signal e "Autoloads cached"))
          (condition-case err
              (progn
                (let ((load-source-file-function nil)) (load autoloads nil 'nomessage))
                (elpaca--signal e "Package activated" 'activated))
            ((error) (elpaca--signal
                      e (format "Failed to load %S: %S" autoloads err) 'failed-to-activate))))
      (and (stringp key) (elpaca--signal e (concat "nonexistent :autoloads file \"" key "\"")))))
  (elpaca--continue-build e))

(defun elpaca--byte-compile (e)
  "Byte compile E's package."
  ;; Assumes all dependencies are 'built
  (elpaca--signal e "Byte compiling" 'byte-compilation)
  (let* ((default-directory (elpaca<-build-dir e))
         (dependency-dirs
          (cl-loop for id in (elpaca-dependencies (elpaca<-id e) '(emacs))
                   for dep = (elpaca-get id)
                   for build-dir = (and dep (elpaca<-build-dir dep))
                   when build-dir collect build-dir)))
    (elpaca-with-emacs e
      (:name "Byte compilation")
      ,@(when (boundp 'native-comp-eln-load-path)
          `((setq native-comp-eln-load-path ',native-comp-eln-load-path)))
      (dolist (dir ',(cons default-directory dependency-dirs))
        (let ((default-directory dir))
          (add-to-list 'load-path dir)
          (normal-top-level-add-subdirs-to-load-path)))
      (byte-recompile-directory ,default-directory 0 'force))))

;;;###autoload
(defun elpaca-dependencies (id &optional ignore interactive recurse)
  "Return recursive list of dependencies queued E with ID.
IGNORE may be a list of symbols which are not included in the resulting list.
RECURSE is used to track recursive calls.
When INTERACTIVE is non-nil, message the list of dependencies."
  (interactive (list (elpaca--read-queued "Dependencies of: ") nil t))
  (if-let* ((e (elpaca-get id))
            (dependencies (elpaca--dependencies e))
            (transitives (cl-loop for (d . _) in dependencies
                                  unless (memq d ignore) collect
                                  (cons d (elpaca-dependencies d (cons d ignore) nil t))))
            (deps (delete-dups (flatten-tree transitives))))
      (if interactive (message "%s" deps) deps)
    (when recurse id)))

(defun elpaca--dependents (id &optional noerror)
  "Return list of packages which depend on queued E with ID.
If NOERROR is non-nil, ignore E's for which dependencies cannot be determined."
  (delete-dups (cl-loop for (i . _) in (elpaca--queued)
                        for deps = (if noerror (ignore-errors (elpaca-dependencies i))
                                     (elpaca-dependencies i))
                        when (memq id deps) collect i)))
;;;###autoload
(defun elpaca-dependents (id &optional message)
  "Return recursive list of packages which depend on queued E with ID.
When MESSAGE is non-nil, message the list of dependents."
  (interactive (list (elpaca--read-queued
                      "Dependents of: "
                      (cl-remove-if-not #'elpaca--dependents (elpaca--queued) :key #'car))
                     t))
  (if message (message "%S" (elpaca--dependents id)) (elpaca--dependents id)))

(defcustom elpaca-interactive-interval 0.05
  "Time to wait before processing queues when multiple `elpaca' forms evaluated."
  :type 'number)
(defcustom elpaca-wait-interval 0.01 "Seconds between `elpaca-wait' status checks."
  :type 'number)
(defvar elpaca--interactive-timer nil
  "Debounces interactive evaluation of multiple `elpaca' forms.")

;;;; COMMANDS/MACROS
;;;###autoload
(defun elpaca-wait ()
  "Block until currently queued orders are processed.
When quit with \\[keyboard-quit], running sub-processes are not stopped."
  (when-let* ((q (cl-loop for q in elpaca--queues thereis
                          (and (eq (elpaca-q<-status q) 'incomplete)
                               (elpaca-q<-elpacas q) q))))
    (setq elpaca--waiting t mode-line-process "elpaca-wait...")
    (elpaca-process-queues)
    (condition-case nil
        (while (not (eq (elpaca-q<-status q) 'complete))
          (discard-input)
          (sit-for elpaca-wait-interval))
      (quit (cl-loop for (_ . e) in (elpaca-q<-elpacas q) do
                     (or (eq (elpaca--status e) 'finished) (elpaca--fail e "User quit")))))
    (elpaca-split-queue)
    (setq elpaca--waiting nil mode-line-process nil)))

(defun elpaca--unprocess (e)
  "Mark E as unprocessed in its queue."
  (let ((q (elpaca--q e)))
    (setf (elpaca<-statuses e) nil
          (elpaca<-builtp e) nil
          (elpaca<-dependencies e) nil
          (elpaca<-queue-time e) (current-time))
    (when (> (elpaca-q<-processed q) 0) (cl-decf (elpaca-q<-processed q)))
    (setf (elpaca-q<-status q) 'incomplete)))

(defun elpaca--expand-declaration (order body)
  "Expand ORDER declaration, deferring BODY."
  (unless order (signal 'wrong-type-argument '((or symbolp consp) nil)))
  (when (memq (car-safe order) '(quote \`)) (setq order (eval order t)))
  (let* ((id (elpaca--first order))
         (q (or (and after-init-time (elpaca--q (elpaca-get id))) (car elpaca--queues)))
         (e (elpaca--queue order q)))
    (when body (setf (alist-get id (elpaca-q<-forms q)) body))
    (when after-init-time
      (elpaca--maybe-log)
      (unless (eq (elpaca--status e) 'failed)
        (elpaca--unprocess e)
        (push 'queued (elpaca<-statuses e))))
    (cond ((plist-get (elpaca<-recipe e) :wait) (elpaca-wait))
          (real-this-command ; called interactively
           (when elpaca--interactive-timer (cancel-timer elpaca--interactive-timer))
           (setq elpaca--interactive-timer
                 (run-at-time elpaca-interactive-interval nil #'elpaca-process-queues))))))

;;;###autoload
(defmacro elpaca (order &rest body)
  "Queue ORDER for asynchronous installation/activation.
Evaluate BODY forms synchronously once ORDER's queue is processed.
See Info node `(elpaca) Basic Concepts'."
  (declare (indent 1) (debug form))
  `(elpaca--expand-declaration ',order ',body))

(defvar elpaca--try-package-history nil "History for `elpaca-try'.")
;;;###autoload
(defun elpaca-try (order &optional interactive)
  "Try ORDER.
Install the repo/build files on disk.
Activate the corresponding package for the current session.
ORDER's package is not activated during subsequent sessions, but still on disk.
When INTERACTIVE is non-nil, immediately process ORDER, otherwise queue ORDER."
  (interactive
   (list (if (equal current-prefix-arg '(4))
             (minibuffer-with-setup-hook #'backward-char
               (read (read-string "elpaca-try: " "()" 'elpaca--try-package-history)))
           (if-let* ((menus (cl-remove 'elpaca-menu-declarations elpaca-menu-functions))
                     (elpaca-menu-functions (list (lambda (&rest _)
                                                    (cl-loop for menu in menus append
                                                             (cl-remove-if #'elpaca-get (funcall menu 'index) :key #'car)))))
                     (item (elpaca-menu-item)))
               (cons (car item) (plist-get (cdr item) :recipe))
             (user-error "No menu item")))
         t))
  (if (not interactive)
      (elpaca--queue order)
    (elpaca--maybe-log)
    (elpaca-queue (eval `(elpaca ,order) t))
    (elpaca--process-queue (nth 1 elpaca--queues)))
  nil)

(defun elpaca--process (e)
  "Process E."
  (when (eq (elpaca--status e) 'queued) (elpaca--continue-build e)))

(defun elpaca--process-queue (q)
  "Process elpacas in Q."
  (cond
   ((eq (elpaca-q<-status q) 'complete)
    (when-let* ((next (nth (1+ (elpaca-q<-id q)) (reverse elpaca--queues))))
      (elpaca--process-queue next)))
   (t (mapc #'elpaca--process (reverse (mapcar #'cdr (elpaca-q<-elpacas q)))))))

(defun elpaca--maybe-reset-queue (q)
  "Reset Q containing incomplete orders."
  (let* ((es (elpaca-q<-elpacas q))
         (ps (cl-loop for (_ . e) in es count (memq (elpaca--status e) '(finished failed)))))
    (setf (elpaca-q<-processed q) ps
          (elpaca-q<-status q) (if (and (> ps 0) (= ps (length es))) 'complete 'incomplete))))

;;;###autoload
(defun elpaca-process-queues (&optional filter)
  "Process the incomplete queues.
FILTER must be a unary function which accepts and returns a queue list."
  (when (and after-init-time elpaca--debug-init) (setq debug-on-error t elpaca--debug-init nil))
  (if-let* ((queues (if filter (funcall filter (reverse elpaca--queues))
                      (reverse elpaca--queues)))
            ((mapc #'elpaca--maybe-reset-queue queues))
            (incomplete (cl-find 'incomplete queues :key #'elpaca-q<-status))
            ((elpaca-q<-elpacas incomplete)))
      (progn (setq elpaca--status-counts (elpaca--count-statuses))
             (elpaca--maybe-log)
             (elpaca--process-queue incomplete))
    (unless elpaca-after-init-time
      (setf elpaca-after-init-time (current-time))
      (run-hooks 'elpaca-after-init-hook))
    (run-hooks 'elpaca--post-queues-hook)))

(defun elpaca--on-disk-p (id)
  "Return t if ID has an associated E with a build or repo dir on disk."
  (when-let* ((e (elpaca-get id)))
    (or (when-let* ((repo (elpaca<-repo-dir e))) (file-exists-p repo))
        (when-let* ((build (elpaca<-repo-dir e))) (file-exists-p build)))))

;;@MAYBE: Should this delete user's declared package if it is a dependency?
;;@MAYBE: user option for deletion policy when repo is dirty.
;;;###autoload
(defun elpaca-delete (id &optional force deps ignored)
  "Remove a package associated with ID from cache and disk.
If DEPS is non-nil (interactively with \\[universal-argument]) delete dependencies.
If FORCE is non-nil (interactively with \\[universal-argument] \\[universal-argument])
do not confirm before deleting package and DEPS."
  (interactive (list (elpaca--read-queued "Delete Package: ")
                     (equal current-prefix-arg '(16))
                     (member current-prefix-arg '((4) (16)))))
  (let* ((e (let ((e (or (elpaca-get id)
                         (ignore-errors (elpaca<-create id)))))
              (if (or (null e) (equal (elpaca--status e) 'struct-failed)) ;; Orphaned packages
                  (setf (alist-get id (elpaca-q<-elpacas (elpaca--q e)))
                        (elpaca<-create (list id :url (symbol-name id) :main nil)))
                e)))
         (repo-dir     (elpaca<-repo-dir e))
         (build-dir    (elpaca<-build-dir e))
         (dependents   (ignore-errors (elpaca-dependents id)))
         (dependencies (and deps (ignore-errors (elpaca-dependencies
                                                 id elpaca-ignored-dependencies)))))
    (when (cl-some #'elpaca--on-disk-p dependents)
      (user-error "Cannot delete %S unless dependents %S are deleted" id dependents))
    (when (or force (yes-or-no-p (format "Delete package %S? " id)))
      (cl-assert (not (member repo-dir (list user-emacs-directory elpaca-builds-directory
                                             elpaca-repos-directory))))
      (when (file-exists-p repo-dir) (delete-directory repo-dir 'recursive))
      (when (file-exists-p build-dir)
        (setq load-path (delete build-dir load-path))
        (delete-directory build-dir 'recursive))
      (dolist (queue elpaca--queues)
        (setf (elpaca-q<-elpacas queue)
              (cl-remove id (elpaca-q<-elpacas queue) :key #'car)))
      (setf (alist-get (car (cl-find (file-name-base (directory-file-name repo-dir))
                                     elpaca--repo-dirs :key #'cadr :test #'equal))
                       elpaca--repo-dirs nil t)
            nil)
      (message "Deleted package %S" id)
      (dolist (dependency dependencies nil)
        (elpaca-delete dependency 'force deps (push id ignored))))))

(defun elpaca--file-package (&optional file)
  "Return queued E if current buffer's FILE is part of a repo, nil otherwise."
  (when-let* ((name (or file (buffer-file-name))))
    (cl-find-if (lambda (e) (assoc name (elpaca--files e)))
                (reverse (elpaca--queued)) :key #'cdr)))

(defun elpaca--read-queued (&optional prompt queued)
  "Read QUEUED ID with PROMPT."
  (intern (completing-read
           (or prompt "Queued item: ")
           (sort (cl-delete-duplicates (mapcar #'car (or queued (elpaca--queued)))) #'string<)
           nil t)))

;;;###autoload
(defun elpaca-rebuild (id &optional interactive)
  "Rebuild ID's associated package.
When INTERACTIVE is non-nil, prompt for ID, immediately process.
With a prefix argument, rebuild current file's package or prompt if none found."
  (interactive (list (or (and-let* ((current-prefix-arg)
                                    (queued (elpaca--file-package))
                                    ((car queued))))
                         (elpaca--read-queued "Rebuild package: "))
                     t))
  (let ((e (or (elpaca-get id) (user-error "Package %S is not queued" id))))
    (when (eq (elpaca--status e) 'finished)
      ;;@MAYBE: remove Info/load-path entries?
      (setf (elpaca<-build-steps e)
            (cl-set-difference (elpaca--build-steps (elpaca<-recipe e))
                               '(elpaca--clone
                                 elpaca--configure-remotes
                                 elpaca--fetch
                                 elpaca--checkout-ref
                                 elpaca--queue-dependencies
                                 elpaca--activate-package))))
    (elpaca--unprocess e)
    (elpaca--signal e "Rebuilding" 'queued)
    (setf elpaca-cache-autoloads nil (elpaca<-files e) nil)
    (when interactive (elpaca-process-queues))))

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
      :sentinel (lambda (process event) (elpaca--process-sentinel nil nil process event)))))

(defun elpaca--fetch (e &rest command)
  "Fetch E's remotes' commits.
COMMAND must satisfy `elpaca--make-process' :command SPEC arg, which see."
  (elpaca--signal e "Fetching remotes" 'fetching-remotes)
  (let ((default-directory (elpaca<-repo-dir e)))
    (elpaca--make-process e
      :name "fetch"
      :command  (or command '("git" "fetch" "--all" "-v"))
      :sentinel (lambda (process event) (elpaca--process-sentinel "Remotes fetched" nil process event)))))

(defun elpaca--announce-pin (e)
  "Log that pinned E is being skipped."
  (elpaca--continue-build e "Skipping pinned package" 'pinned))

(defun elpaca--pinned-p (e)
  "Return pin description of form (ID keyword . val) if E is pinned."
  (cl-loop with recipe = (elpaca<-recipe e) for keyword in '(:pin :tag :ref)
           thereis (when-let* ((member (plist-member recipe keyword))
                               (val (cadr member)))
                     (list (elpaca<-id e) (car member) val))))

(defun elpaca-pinned-p (e)
  "Return non-nil if E's package is pinned."
  (cl-loop with repo = (elpaca<-repo-dir e)
           initially (when-let* ((pinnedp (elpaca--pinned-p e))) (cl-return pinnedp))
           for (_ . e2) in (elpaca--queued) thereis (and (equal (elpaca<-repo-dir e2) repo)
                                                         (elpaca--pinned-p e2))))

;;;###autoload
(defun elpaca-fetch (id &optional interactive)
  "Fetch ID's associated package remote commits.
This does not merge changes or rebuild the packages.
If INTERACTIVE is non-nil immediately process, otherwise queue."
  (interactive (list (elpaca--read-queued "Fetch Package Updates: ") t))
  (let ((e (or (elpaca-get id) (user-error "Package %S is not queued" id))))
    (elpaca--unprocess e)
    (elpaca--signal e nil 'queued)
    (setf (elpaca<-build-steps e) (if (elpaca-pinned-p e) (list #'elpaca--announce-pin)
                                    (list #'elpaca--fetch #'elpaca--log-updates)))
    (when interactive
      (elpaca--maybe-log)
      (elpaca--process e))))

;;;###autoload
(defun elpaca-fetch-all (&optional interactive)
  "Fetch queued elpaca remotes.  If INTERACTIVE is non-nil, process queues."
  (interactive (list t))
  (cl-loop with repos
           for (id . e) in (reverse (elpaca--queued))
           for repo = (elpaca<-repo-dir e)
           for mono-repo = (alist-get repo repos nil nil #'equal)
           do (if mono-repo ; skip queued mono-repos
                  (setf (elpaca<-build-steps e)
                        `((lambda (e)
                            (elpaca--continue-build e ,(format "Mono-repo fetched by %s" mono-repo))))
                        (elpaca<-statuses e) (list 'queued))
                (elpaca-fetch id))
           (unless mono-repo (push (cons (elpaca<-repo-dir e) id) repos)))
  (when interactive (elpaca-process-queues)))

(defun elpaca--merge-process-sentinel (process _event)
  "Handle PROCESS EVENT."
  (if-let* ((e (process-get process :elpaca))
            ((= (process-exit-status process) 0))
            (repo (elpaca<-repo-dir e))
            (default-directory repo))
      (progn (when (equal (elpaca-process-output "git" "rev-parse" "HEAD")
                          (process-get process :elpaca-git-rev))
               (cl-loop for (_ . d) in (elpaca--queued)
                        when (equal (elpaca<-repo-dir d) repo) do
                        (setf (elpaca<-build-steps d) nil)))
             (elpaca--propertize-subprocess process)
             (elpaca--continue-build e))
    (elpaca--fail e "Merge failed")))

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

;;;###autoload
(defun elpaca-merge (id &optional fetch interactive)
  "Merge package commits associated with ID.
If FETCH is non-nil, download package changes before merging.
If INTERACTIVE is non-nil, the queued order is processed immediately."
  (interactive (list (elpaca--read-queued "Merge package: ") current-prefix-arg t))
  (let* ((e (or (elpaca-get id) (user-error "Package %S is not queued" id)))
         (recipe (elpaca<-recipe e)))
    (elpaca--unprocess e)
    (setf (elpaca<-build-steps e)
          (if (elpaca-pinned-p e) (list #'elpaca--announce-pin)
            `(,@(when fetch '(elpaca--fetch elpaca--log-updates))
              elpaca--merge
              ,@(cl-set-difference
                 (elpaca--build-steps recipe nil 'cloned (elpaca<-mono-repo e))
                 '(elpaca--configure-remotes
                   elpaca--fetch
                   elpaca--checkout-ref
                   elpaca--queue-dependencies
                   elpaca--activate-package))))
          (elpaca<-statuses e) (list 'queued))
    (when interactive
      (elpaca--maybe-log)
      (elpaca--process e))))

;;;###autoload
(defun elpaca-pull (id &optional interactive) ;;@MAYBE: optional REBUILD arg?
  "Fetch, merge, and rebuild package associated with ID.
If INTERACTIVE is non-nil, process queues."
  (interactive (list (elpaca--read-queued "Update package: ") t))
  (elpaca-merge id 'fetch interactive))
(defalias 'elpaca-update #'elpaca-pull)

;;;###autoload
(defun elpaca-merge-all (&optional fetch interactive)
  "Merge and rebuild queued packages.
If FETCH is non-nil fetch updates first.
If INTERACTIVE is non-nil, process queues."
  (interactive (list current-prefix-arg t))
  (cl-loop with (seen repos)
           with ignored = (remove 'elpaca elpaca-ignored-dependencies)
           for (id . e) in (reverse (elpaca--queued))
           unless (memq id seen) do
           (let* ((repo (elpaca<-repo-dir e))
                  (mono-repo (alist-get repo repos nil nil #'equal))
                  (deps (elpaca-dependencies id ignored)))
             (elpaca-merge id fetch)
             (if (not mono-repo)
                 (elpaca--signal e "Updating" (when (elpaca<-blockers e) 'blocked))
               (cl-pushnew id (elpaca<-blocking (elpaca-get mono-repo)))
               (cl-pushnew mono-repo (elpaca<-blockers e))
               (when (cdr deps) (push 'blocked-by-mono-repo (elpaca<-statuses e)))
               (elpaca--signal e (format "Waiting for mono-repo %s" mono-repo) 'blocked))
             (push id seen)
             (unless mono-repo (push (cons repo id) repos))))
  (when interactive (elpaca-process-queues)))

;;;###autoload
(defun elpaca-pull-all (&optional interactive)
  "Update all queued packages. If INTERACTIVE is non-nil, process queue."
  (interactive (list t))
  (elpaca-merge-all 'fetch interactive))
(defalias 'elpaca-update-all #'elpaca-pull-all)

;;; Lockfiles
(defun elpaca-declared-p (id)
  "Return t if ID is declared in user's init file, nil otherwise."
  (when-let* ((e (elpaca-get id)))
    (or (elpaca<-init e) (cl-loop for dependent in (elpaca-dependents id)
                                  when (elpaca-declared-p dependent) return t))))

(defun elpaca-installed-p (id)
  "Return t if ID's associated repo directory is on disk, nil otherwise."
  (and-let* ((e (elpaca-get id))
             (repo-dir (elpaca<-repo-dir e))
             ((file-exists-p repo-dir)))))

(defun elpaca-worktree-dirty-p (id)
  "Return t if ID's associated repository has a dirty worktree, nil otherwise."
  (when-let* ((e (elpaca-get id))
              (recipe (elpaca<-recipe e))
              (repo-dir (elpaca<-repo-dir e))
              ((file-exists-p repo-dir))
              (default-directory repo-dir))
    (not (string-empty-p (elpaca-process-output
                          "git" "-c" "status.branch=false" "status" "--short")))))

(defcustom elpaca-lock-file nil "Path of `elpaca-menu-lock-file' cache." :type 'file)

(defvar elpaca-menu-lock-file--cache nil)
(defun elpaca-menu-lock-file (request &optional item)
  "If REQUEST is `index`, return `elpaca-lock-file' ITEM, otherwise update menu."
  (when elpaca-lock-file
    (if-let* (((eq request 'index))
              (cache (or elpaca-menu-lock-file--cache (elpaca-menu-lock-file 'update))))
        (if item (alist-get item cache) cache)
      (setq elpaca-menu-lock-file--cache (elpaca--read-file elpaca-lock-file)))))

(defcustom elpaca-lock-file-functions (list #'elpaca<-init)
  "List of functions which take an E as a first argument.
Any function returning nil will prevent the E from being written to the file."
  :type 'hook)

;;;###autoload
(defmacro elpaca-with-dir (e type &rest body)
  "Evaluate BODY with E's `default-directory' bound.
TYPE is either `repo` or `build`, for repo or build directory."
  (declare (indent 2) (debug (symbolp symbolp &rest form)))
  `(let ((default-directory (,(intern (format "elpaca<-%s-dir" (symbol-name type))) ,e)))
     ,@body))

;;;###autoload
(defmacro elpaca-defscript (name args &rest body)
  "Define a scripting build step function and return its NAME.
The ARGS plist must contain one of the following values for the :type key:
- `elisp`: run BODY in an Emacs subprocess.
   ARGS are passed to `elpaca-with-emacs', which see.
- `system`: each form in BODY is interepreted as (PROGRAM [ARGS...]).
In addition, the ARGS `:dir` may specify the package `build` or `source` dir."
  (declare (indent defun))
  (if-let* ((type (plist-get args :type))
            ((memq type '(elisp system))))
      `(defun ,name (e)
         "Run E's scripted steps. Generated by `elpaca-defstep'."
         ,@(if (eq type 'system)
               `((cl-loop
                  with steps = ',(reverse body)
                  with limit = (length steps)
                  for i below limit
                  for step = (nth i steps)
                  for stepname = (format "%s-%s" ',name (- limit i))
                  do (push `(lambda (e)
                              (elpaca-with-dir e ,',(or (plist-get args :dir) 'source)
                                (elpaca--signal e ,stepname 'script)
                                (elpaca--make-process e
                                  :name ,stepname
                                  :buffer ,stepname
                                  :command (backquote ,,'step)
                                  :sentinel
                                  (lambda (process event)
                                    (elpaca--process-sentinel nil nil process event)))))
                           (elpaca<-build-steps e))
                  finally (elpaca--continue-build e)))
             `((elpaca-with-dir e ,(or (plist-get args :dir) 'source)
                 (elpaca--signal e ,(format "%s" name) 'script)
                 (elpaca-with-emacs e ,args ,@body)))))
    (signal 'wrong-type-argument `((memq '(elisp script)) ,type))))

;;;###autoload
(defun elpaca-write-lock-file (path &optional elpacas)
  "Write lock file to PATH for current state of queued ELPACAS."
  (interactive "FWrite lock-file to: ")
  (elpaca--write-file path
    (cl-loop
     with seen with es
     for (id . e) in (or elpacas (elpaca--queued))
     do (when (and (not (member id seen))
                   (run-hook-with-args-until-failure 'elpaca-lock-file-functions e))
          (push `(,id
                  :source "elpaca-menu-lock-file"
                  :recipe
                  ,(plist-put (copy-tree (elpaca<-recipe e))
                              :ref
                              (elpaca-with-dir e repo
                                (elpaca-with-process-call ("git" "rev-parse" "HEAD")
                                  (if success (string-trim stdout)
                                    (error "Unable to write lock-file: %s %S" id stderr))))))
                es)
          (push id seen))
     finally (message "wrote %d elpacas to %s" (length es) path)
     (pp (cl-sort es #'string< :key #'car)))))

(declare-function elpaca-ui-current-package "elpaca-ui")
;;;###autoload
(defun elpaca-visit (&optional id build)
  "Open local repository directory for E with ID.
When BUILD is non-nil visit build directory."
  (interactive (list (elpaca--read-queued
                      (format "Visit %s dir: " (if current-prefix-arg "build" "repo")))
                     current-prefix-arg))
  (when (eq id '##) (setq id nil)) ; Empty `elpaca--read-queued' response
  (if (not id)
      (find-file (if build elpaca-builds-directory elpaca-repos-directory))
    (if-let* ((e (elpaca-get id))
              (dir (if build (elpaca<-build-dir e) (elpaca<-repo-dir e))))
        (if (file-exists-p dir)
            (find-file dir)
          (user-error "Directory does not exist: %S" dir))
      (user-error "%S is not a queued package" id))))

(defun elpaca--fallback-date (e)
  "Return time of last modification for E's built elisp, otherwise nil."
  (file-attribute-modification-time
   (file-attributes (expand-file-name (concat (elpaca<-package e) ".el")
                                      (elpaca<-repo-dir e)))))

(defun elpaca-menu-declarations (request &optional item)
  "Return menu ITEM REQUEST for orders with no recipe in `elpaca-menu-functions'.
This should only ever be used as the last element of `elpaca-menu-functions'."
  (when (and after-init-time (eq request 'index))
    (cl-loop for (id . e) in (elpaca--queued)
             with elpaca-menu-functions = (cl-remove 'elpaca-menu-declarations elpaca-menu-functions)
             when (not (elpaca-menu-item id))
             collect (list id :source (if (elpaca<-init e) "Init file" "User Declaration")
                           :date (ignore-errors (elpaca--fallback-date e))
                           :recipe (elpaca<-recipe e)
                           :description (concat "Declared " (if (elpaca<-init e) "in Init file" "by user")))
             into items finally return (if item (elpaca-alist-get item items) items))))

;;;###autoload
(defun elpaca-browse (id)
  "Browse menu item with ID's :url." ;;@Doc: we also fall back to recipe :url
  (interactive (list (let* ((elpaca-overriding-prompt "Browse package: ")
                            ;;@TODO: include init file packages
                            (recipe (elpaca-recipe)))
                       (intern (plist-get recipe :package)))))
  (if-let* ((found (or (elpaca-get id)
                       (alist-get id (cl-loop for menu in elpaca-menu-functions append (funcall menu 'index)))))
            (url (or (plist-get found :url)
                     (file-name-sans-extension
                      (elpaca--repo-uri (elpaca-merge-plists
                                         (or (plist-get found :recipe)
                                             (and (elpaca-p found) (elpaca<-recipe found))
                                             (user-error "No URL associated with id %S" id))
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
         (info (list (cons 'elpaca repo) (cons 'installer elpaca-installer-version)
                     (cons 'emacs (emacs-version)) (cons 'git git))))
    (when (member output '(message string))
      (setq info (format "Elpaca %s\ninstaller:      %S\nemacs-version:  %s\ngit --version:  %s"
                         repo elpaca-installer-version (emacs-version) git)))
    (if (eq output 'message) (message "%s" info) info)))

(provide 'elpaca)
;;; elpaca.el ends here
