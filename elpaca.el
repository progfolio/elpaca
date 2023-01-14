;;; elpaca.el --- An elisp package manager           -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nicholas Vollmer

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
(unless (executable-find "git") (error "Elpaca unable to find git executable"))

(defgroup elpaca nil
  "An elisp package manager."
  :group 'elpaca
  :prefix "elpaca-")

(defface elpaca-finished '((t (:weight bold :foreground "#00FF00")))
  "Indicates an order is finished.")

(defface elpaca-blocked '((t (:weight bold :foreground "#FFF01F")))
  "Indicates an order is blocked.")

(defface elpaca-failed '((t (:weight bold :foreground "#FF1818")))
  "Indicates an order has failed.")

(defcustom elpaca-status-faces '((blocked  . elpaca-blocked)
                                 (finished . elpaca-finished)
                                 (failed   . elpaca-failed))
  "Alist mapping order statuses to faces."
  :type 'alist)

(defvar elpaca--info-timer nil "Timer to debounce order info printing.")

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

(defcustom elpaca-cache-autoloads t
  "If non-nil, cache package autoloads and load all at once.
Results in faster start-up time."
  :type 'boolean)

(defcustom elpaca-directory (expand-file-name "elpaca" user-emacs-directory)
  "Location of the elpaca package store."
  :type 'directory)

(defvar elpaca-cache-directory (expand-file-name "cache" elpaca-directory)
  "Location of the cache directory.")

(defvar elpaca-builds-directory (expand-file-name "builds" elpaca-directory)
  "Location of the builds directory.")

(defcustom elpaca-makeinfo-executable (executable-find "makeinfo")
  "Path of the makeinfo executable."
  :type 'string)

(defcustom elpaca-install-info-executable (executable-find "install-info")
  "Path of the install-info executable."
  :type 'string)

(defcustom elpaca-info-timer-interval 0.02
  "Number of idle seconds to wait before printing order statuses.
Setting this to too low may cause the status buffer to block more.
Setting it too high causes prints fewer status updates."
  :type 'number)

(defcustom elpaca--process-busy-interval 60
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
  :type 'list)

(defvar elpaca--debug-init debug-on-error "Preserves --debug-init option.")

(defvar elpaca-default-files-directive
  '("*.el" "*.el.in" "dir" "*.info" "*.texi" "*.texinfo"
    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
              "README*" "*-pkg.el"))
  "Default value for the `:files' directive in recipes.
It is also spliced in at any point where the `:defaults' keyword
is used in a `:files' directive.")

(defvar elpaca-order-defaults (list :protocol 'https :remotes "origin" :inherit t :depth 1)
  "Default order modifications.")

(defun elpaca-order-defaults (_order) "Matches any order." elpaca-order-defaults)

(defcustom elpaca-order-functions '(elpaca-order-defaults)
  "Abnormal hook run to alter orders.
Each element must be a unary function which accepts an order.
An order may be nil, a symbol naming a package, or a plist.
The function may return nil or a plist to be merged with the order.
This hook is run via `run-hook-with-args-until-success'."
  :type 'hook)

(defun elpaca-recipe-defaults (recipe)
  "Default RECIPE modifications. Matches any RECIPE."
  (let ((plist))
    (unless (plist-get recipe :files)
      (push (list :defaults) plist)
      (push :files plist))
    (when-let ((url (plist-get recipe :url))
               ((string-match-p "depp.brause.cc" url)))
      (push nil plist)
      (push :depth plist))
    (when (equal (plist-get recipe :package) "org")
      (push nil plist)
      (push :depth plist))
    plist))

(defcustom elpaca-recipe-functions '(elpaca-recipe-defaults)
  "Abnormal hook run to alter recipes.
Each element must be a unary function which accepts an recipe plist.
The function may return nil or a plist to be merged with the recipe.
This hook is run via `run-hook-with-args-until-success'."
  :type 'hook)

(defcustom elpaca-menu-functions
  '(elpaca-menu-org elpaca-menu-melpa elpaca-menu-gnu-elpa-mirror elpaca-menu-non-gnu-elpa)
  "Abnormal hook to lookup packages in menus.
Each function is passed a request, which may be any of the follwoing symbols:
  - `index`
     Must return a alist of the menu's package candidates.
     Each candidate is a cell of form:
     (PACKAGE-NAME . (:source SOURCE-NAME :recipe RECIPE-PLIST))
  - `update`
     Updates the menu's package candidate list."
  :type 'hook)

(defcustom elpaca-hide-initial-build nil
  "When non-nil, hide `elpaca-log' during init time builds." :type 'boolean)
(defvar elpaca--ibc initial-buffer-choice "User's `initial-buffer-choice'.")
(defun elpaca--set-ibc (_ new &rest _)
  "Update `elpaca--ibc' when `initial-buffer-choice' set to NEW."
  (unless (eq new #'elpaca--ibs) (setq elpaca--ibc new)))
(add-variable-watcher 'initial-buffer-choice #'elpaca--set-ibc)

(defcustom elpaca-verbosity 0 "Maximum event verbosity level shown in logs."
  :type 'integer)
(defcustom elpaca-default-remote-name "origin" "Default remote name." :type 'string)

(defvar elpaca-ignored-dependencies
  '(emacs cl-lib cl-generic nadvice org org-mode map seq json project auth-source-pass)
  "Ignore these unless the user explicitly requests they be installed.")

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
  (type (unless after-init-time 'init))
  (id  (if (boundp 'elpaca--queues) (length elpaca--queues) 0))
  (processed 0)
  (status 'incomplete)
  (time (current-time))
  pre post autoloads forms elpacas)

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
If CACHE may be any of the following symbols:
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

;;@TODO: clean up interface.
;;;###autoload
(defun elpaca-menu-item (&optional interactive symbol menus filter no-descriptions)
  "Return menu item matching SYMBOL in MENUS or `elpaca-menu-functions'.
If SYMBOL is nil, prompt for it.
If INTERACTIVE is equivalent to \\[universal-argument] prompt for MENUS.
FILTER must be a function which accepts a candidate.
If it returns nil, the candidate is not considered for selection.
If NO-DESCRIPTIONS is non-nil, candidate descriptions are not included.
This is faster (what you want with non-interactive calls)."
  (interactive "P")
  (let* ((omenus menus)
         (menus (if (equal interactive '(4))
                    (mapcar #'intern-soft
                            (cl-remove-duplicates
                             (completing-read-multiple "Menus: " elpaca-menu-functions
                                                       nil t)
                             :test #'equal))
                  (or menus elpaca-menu-functions (user-error "No menus found"))))
         (candidates
          (let ((c (if filter
                       (cl-remove-if-not filter (elpaca--menu-items (not omenus) menus))
                     (elpaca--menu-items (not omenus) menus))))
            (if no-descriptions
                c
              (mapcar (lambda (candidate)
                        (propertize
                         (format "%-30s %s" (car candidate)
                                 (or (plist-get (cdr candidate) :description) ""))
                         'candidate candidate))
                      c))))
         (symbol (or symbol
                     (intern
                      (let ((choice
                             (completing-read (or elpaca-overriding-prompt "Package: ")
                                              candidates nil t)))
                        (if no-descriptions
                            choice
                          (car (split-string choice "\\(?:[[:space:]]+\\)")))))))
         (candidate (elpaca-alist-get symbol
                                      (if no-descriptions
                                          candidates
                                        (mapcar (lambda (c) (get-text-property 0 'candidate c))
                                                candidates))))
         (recipe (plist-get candidate :recipe)))
    (if (called-interactively-p 'interactive)
        (progn
          (unless recipe (user-error "No menu recipe for %s" symbol))
          (kill-new (format "%S" recipe))
          (message "%S menu recipe for %s copied to kill ring:\n%S"
                   (plist-get candidate :source) symbol recipe))
      recipe)))

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
(defun elpaca-recipe (order &optional interactive)
  "Return recipe computed from ORDER.
ORDER is any of the following values:
  - nil. The order is prompted for.
  - an item symbol which will be looked up via `elpaca-menu-functions'
  - an order list of the form: //='(ITEM . PROPS).
When INTERACTIVE is non-nil, `yank' the recipe to the clipboard."
  (interactive (list (if-let ((elpaca-overriding-prompt "Recipe: ")
                              (recipe (elpaca-menu-item)))
                         (push (intern (plist-get recipe :package)) recipe)
                       (user-error "No recipe selected"))
                     t))
  (let* ((props (cdr-safe order))
         (item (elpaca--first order))
         (nonheritablep (elpaca--inheritance-disabled-p props))
         (mods (unless nonheritablep (run-hook-with-args-until-success
                                      'elpaca-order-functions order)))
         (menu-item (unless (or interactive ;; we already queried for this.
                                (elpaca--inheritance-disabled-p
                                 (elpaca-merge-plists
                                  mods (plist-member props :inherit))))
                      (elpaca-menu-item nil item nil nil 'no-descriptions)))
         (recipe (elpaca-merge-plists menu-item mods props)))
    (unless (plist-get recipe :package)
      (setq recipe (plist-put recipe :package (symbol-name item))))
    (when-let ((recipe-mods (run-hook-with-args-until-success
                             'elpaca-recipe-functions recipe)))
      (setq recipe (elpaca-merge-plists recipe recipe-mods)))
    (if (not interactive)
        recipe
      (kill-new (format "%S" recipe))
      (message "%S recipe copied to kill-ring:\n%S" (plist-get recipe :package) recipe))))

(defsubst elpaca--emacs-path ()
  "Return path to running Emacs."
  (concat invocation-directory invocation-name))

(defsubst elpaca--repo-name (string)
  "Return repo name portion of STRING."
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
         (info (intern (concat url repo hostname)))
         (mono-repo (alist-get info elpaca--repo-dirs))
         (name (cond
                (local-repo
                 (if (and mono-repo (not (equal (cdr mono-repo) pkg)))
                     (error "Duplicate :local-repo %S" local-repo)
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
         (dir (if (and (not mono-repo) (assoc name (mapcar #'cdr elpaca--repo-dirs)))
                  (string-join (list name hostname user) ".")
                (and name (replace-regexp-in-string "\\.el$" "" name)))))
    (unless mono-repo (push (cons info (cons dir pkg)) elpaca--repo-dirs))
    (file-name-as-directory (expand-file-name (concat "repos/" dir) elpaca-directory))))

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
    (pcase (elpaca--repo-type repo)
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
  id package item statuses
  repo-dir build-dir mono-repo
  files build-steps recipe
  dependencies dependents includes
  (queue-id (1- (length elpaca--queues)))
  (queue-time (current-time))
  (init (not after-init-time))
  process log)

(defmacro elpaca--required-arg (try info)
  "TRY to set arg. If error, fail E with INFO."
  (declare (indent 1) (debug t))
  `(condition-case err ,try
     ((error) (setq status 'struct-failed info (format ,info err)) nil)))

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

;;@TODO: dependenies in mono-repo should not do double work of adding to load-path, info building, etc
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
      (when (and mono-repo (memq 'ref-checked-out (elpaca<-statuses mono-repo)))
        (setq steps
              (cl-set-difference steps '(elpaca--clone elpaca--configure-remotes elpaca--checkout-ref))))
      (when clonedp (setq steps (delq 'elpaca--clone steps)))
      steps)))

(defun elpaca--ibs ()
  "Return initial status buffer if `elpaca-hide-initial-build' is nil."
  (elpaca-log "#unique !finished")
  (when (bound-and-true-p elpaca-log-buffer)
    (with-current-buffer (get-buffer-create elpaca-log-buffer)
      (when (equal initial-buffer-choice #'elpaca--ibs) (setq initial-buffer-choice elpaca--ibc))
      (setq-local elpaca-ui-want-tail nil)
      (current-buffer))))

(cl-defun elpaca<-create
    (item &key recipe repo-dir build-dir files mono-repo)
  "Create a new elpaca struct from ITEM.
Keys are as follows:
  :RECIPE metadata for building package
  :REPO-DIR package's build-dir
  :BUILD-DIR package's repo-dir
  :CACHED whether or not the package was read from the cache
  :FILES list of package's linked files
  :MONO-REPO E which is responsible for cloning repo current E is in."
  (let* ((status 'queued)
         (info "Package queued")
         (id (elpaca--first item))
         (recipe (or recipe (elpaca--required-arg (elpaca-recipe item) "No recipe: %S")))
         (repo-dir
          (or repo-dir (and recipe (elpaca--required-arg (elpaca-repo-dir recipe)
                                     "Unable to determine repo dir: %S"))))
         (build-dir (or build-dir (and recipe (elpaca-build-dir recipe))))
         (clonedp (and repo-dir (file-exists-p repo-dir)))
         (builtp (and clonedp (and build-dir (file-exists-p build-dir))))
         (mono-repo (or mono-repo
                        (when-let (((not builtp))
                                   (e (elpaca--mono-repo id repo-dir)))
                          (setq status 'blocked info (concat "Waiting on monorepo " repo-dir))
                          e)))
         (build-steps (elpaca--build-steps recipe builtp clonedp mono-repo))
         (elpaca (elpaca<--create
                  :id id :package (symbol-name id) :item item :statuses (list status)
                  :repo-dir repo-dir :build-dir build-dir :mono-repo mono-repo
                  :files files :build-steps build-steps :recipe recipe
                  :includes (and mono-repo (list (cadr mono-repo)))
                  :log (list (list status nil info)))))
    (when mono-repo (cl-pushnew id (elpaca<-includes mono-repo)))
    (unless (or builtp elpaca-hide-initial-build elpaca-after-init-time)
      (setq initial-buffer-choice #'elpaca--ibs elpaca-hide-initial-build t))
    elpaca))

(defsubst elpaca--status (e) "Return E's status." (car (elpaca<-statuses e)))

(defun elpaca--signal (e &optional info status replace verbosity)
  "Signal a change to E.
If INFO is non-nil, log and possibly print it in `elpaca-log-buffer'.
If REPLACE is non-nil, E's log is updated instead of appended.
If VERBOSITY is non-nil, log event is given that verbosity number.
If STATUS is non-nil and is not E's current STATUS, signal E's depedentents to
check (and possibly change) their statuses."
  (when-let (((and status (not (eq status (elpaca--status e)))))
             (queued (elpaca--queued)))
    (push status (elpaca<-statuses e))
    (when (memq status '(finished failed blocked))
      (mapc (lambda (d) (elpaca--check-status (elpaca-alist-get d queued)))
            (elpaca<-dependents e)))
    (when (eq status 'failed) (elpaca-log "#unique !finished")))
  (when (memq 'ref-checked-out (elpaca<-statuses e))
    (mapc (lambda (i) (elpaca--continue-mono-repo-dependency (elpaca-alist-get i (elpaca--queued))))
          (elpaca<-includes e)))
  (when info (elpaca--log-event e info verbosity replace))
  (when-let ((verbosity (or verbosity 0))
             ((<= verbosity elpaca-verbosity)))
    (when elpaca--info-timer (cancel-timer elpaca--info-timer))
    (setq elpaca--info-timer
          (and info (run-at-time elpaca-info-timer-interval nil #'elpaca--update-log-buffer)))
    nil))

(defun elpaca--fail (e &optional reason)
  "Fail E for REASON."
  (unless (eq (elpaca--status e) 'failed)
    (let ((item (elpaca<-item e))
          (queue (car (last elpaca--queues (1+ (elpaca<-queue-id e))))))
      (setf (elpaca-q<-forms queue)
            (assq-delete-all (elpaca--first item) (elpaca-q<-forms queue))))
    (elpaca--signal e reason 'failed)
    (elpaca--finalize e)))

(defun elpaca--log-event (e text &optional verbosity replace)
  "Store TEXT in E's log.
Each event is of the form: (STATUS TIME TEXT (or VERBOSITY 0))
If REPLACE is non-nil, the most recent log entry is replaced."
  (let ((event (list (elpaca--status e) (current-time) text (or verbosity 0))))
    (if replace
        (setf (car (elpaca<-log e)) event)
      (push event (elpaca<-log e)))))

(defun elpaca-get-queued (item &optional queue)
  "Return E associated with ITEM from QUEUE."
  (elpaca-alist-get item (or queue (elpaca--queued))))

(defun elpaca--run-build-commands (commands)
  "Run build COMMANDS."
  (dolist (command (if (listp (car-safe commands)) commands (list commands)))
    (message "Running command: %S" command)
    (if (cl-every #'stringp command)
        (apply #'elpaca-process-poll command)
      (eval command t))))

(defsubst elpaca--info (e)
  "Return E's most recent log event info."
  (nth 2 (car (elpaca<-log e))))

(defun elpaca--continue-build (e)
  "Run E's next build step."
  (unless (memq (elpaca--status e) '(blocked failed))
    (let ((fn (or (pop (elpaca<-build-steps e)) #'elpaca--finalize)))
      (condition-case-unless-debug err
          (funcall fn e)
        ((error) (elpaca--fail e (format "%s: %S" fn err)))))))

(defun elpaca--continue-mono-repo-dependency (e)
  "Continue processing E after its mono-repo is in the proper state."
  (when-let ((statuses (elpaca<-statuses e))
             ((not (or (memq 'unblocked-mono-repo statuses)
                       (memq 'ref-checked-out statuses)))))
    (elpaca--remove-build-steps e '(elpaca--clone elpaca--configure-remotes elpaca--checkout-ref))
    (elpaca--signal e nil 'ref-checked-out)
    (elpaca--signal e nil 'unblocked-mono-repo)
    (elpaca--continue-build e)))

(declare-function elpaca-log "elpaca-log")
(declare-function elpaca-status "elpaca-status")
(declare-function elpaca-ui--update-search-filter "elpaca-ui")
(defun elpaca--update-log-buffer ()
  "Update views in `elpaca-log-buffer'."
  (when-let ((log (bound-and-true-p elpaca-log-buffer))
             ((get-buffer-window log t))) ;; log buffer visible
    (with-current-buffer log
      (elpaca-ui--update-search-filter log))))

(defun elpaca--log-duration (e)
  "Return E's log duration."
  (time-subtract (nth 1 (car (elpaca<-log e))) (elpaca<-queue-time e)))

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
  "Execute BODY in new queue with [KEY VAL...] args.
Accepted KEYS are :pre and :post which are hooks run around queue processing."
  (declare (debug t))
  (let* ((pre (when-let ((found (memq :pre body)))
                (butlast found (- (length found) 2))))
         (post (when-let ((found (memq :post body)))
                 (butlast found (- (length found) 2)))))
    (while (keywordp (car body)) (pop body) (pop body))
    `(progn
       (apply #'elpaca-split-queue ',(append pre post))
       ,@body
       (elpaca-split-queue))))

(defun elpaca--finalize-queue (q)
  "Run Q's post isntallation functions:
- load cached autoloads
- evaluate deferred package configuration forms
- possibly run `elpaca-after-init-hook'."
  (when-let ((autoloads (elpaca-q<-autoloads q)))
    (condition-case-unless-debug err
        (eval `(progn ,@autoloads) t)
      ((error) (warn "Autoload Error: %S" err))))
  (cl-loop with forms = (reverse (elpaca-q<-forms q))
           for (item . body) in forms
           do (condition-case-unless-debug err
                  (eval `(progn ,@body) t)
                ((error) (warn "Package Config Error %s: %S" item err))))
  (setf (elpaca-q<-status q) 'complete)
  (let ((next (nth (1+ (elpaca-q<-id q)) (reverse elpaca--queues))))
    (if (and (null elpaca-after-init-time)
             (eq (elpaca-q<-type q) 'init)
             (or (null next)
                 (not (eq (elpaca-q<-type next) 'init))))
        (progn
          (run-hooks 'elpaca-after-init-hook)
          (setq elpaca-after-init-time (current-time))
          (remove-variable-watcher 'initial-buffer-choice #'elpaca--set-ibc)
          (elpaca-split-queue))
      (when-let ((post (elpaca-q<-post q))) (funcall post))
      (run-hooks 'elpaca-post-queue-hook)
      (when next (elpaca--process-queue next)))))

(defun elpaca--finalize (e)
  "Declare E finished or failed."
  (let ((status (elpaca--status e)))
    (if (eq  status 'finished)
        (cl-loop with queued = (elpaca--queued)
                 for item in (elpaca<-dependents e)
                 for dependent = (elpaca-alist-get item queued)
                 unless (eq (elpaca--status dependent) 'finished)
                 do (elpaca--check-status dependent))
      (unless (eq (elpaca--status e) 'failed)
        (elpaca--signal
         e (concat  "✓ " (format-time-string "%s.%3N" (elpaca--log-duration e)) " secs")
         'finished))
      (when-let ((q (car (last elpaca--queues (1+ (elpaca<-queue-id e)))))
                 ((= (cl-incf (elpaca-q<-processed q)) (length (elpaca-q<-elpacas q)))))
        (elpaca--finalize-queue q)))))

(defun elpaca--queue (item)
  "Queue (ITEM . e) in `elpaca--queued'. Return e."
  (if (and (not after-init-time) (elpaca-alist-get item (elpaca--queued)))
      (warn "Duplicate item declaration: %S" item)
    (let* ((e (elpaca<-create item))
           (log (pop (elpaca<-log e)))
           (status (car log))
           (info (nth 2 log)))
      (push (cons (elpaca<-id e) e) (elpaca-q<-elpacas (car elpaca--queues)))
      (if (eq status 'struct-failed)
          (elpaca--fail e info)
        (elpaca--signal e info status))
      e)))

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
      (when (or (stringp remotes) ; Normalize :remotes value
                (not (cl-every (lambda (spec) (or (stringp spec) (listp spec))) remotes)))
        (setq remotes (list remotes)))
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
         (files             (or files (plist-get recipe :files)))
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
  (elpaca--signal e "Build files linked")
  (elpaca--continue-build e))

(defun elpaca--add-info-path (e)
  "Add the E's info to `Info-directory-list'."
  (let ((build-dir (elpaca<-build-dir e)))
    (if (file-exists-p (expand-file-name "dir" build-dir))
        (progn
          (elpaca--signal e "Adding Info path" 'info)
          (with-eval-after-load 'info
            (info-initialize)
            (cl-pushnew build-dir Info-directory-list)))
      (elpaca--signal e "No Info dir file found" 'info))
    (elpaca--continue-build e)))

(defun elpaca--process-busy (process)
  "Update E's status when PROCESS has stopped producing output."
  (when-let (((eq (process-status process) 'run))
             (e (process-get process :elpaca)))
    (elpaca--signal e (process-get process :result) 'blocked)))

(defun elpaca--process-filter (process output &optional pattern status)
  "Filter PROCESS OUTPUT.
PATTERN is a string which is checked against the entire process output.
If it matches, the E associated with process has its STATUS updated."
  (process-put process :raw-output (concat (process-get process :raw-output) output))
  (let* ((e       (process-get process :elpaca))
         (result  (process-get process :result))
         (timer   (process-get process :timer))
         (chunk   (concat result output))
         (lines   (split-string chunk "\n"))
         (returnp (string-match-p "" chunk))
         (linep   (string-empty-p (car (last lines)))))
    (unless (process-get process :messaged)
      (elpaca--signal e (elpaca--command-string (process-command process)) nil nil 1)
      (process-put process :messaged t))
    (when timer (cancel-timer timer))
    (process-put process :timer (run-at-time elpaca--process-busy-interval nil
                                             (lambda () (elpaca--process-busy process))))
    (unless linep
      (process-put process :result (car (last lines)))
      (setq lines (butlast lines)))
    (dolist (line lines)
      (unless (string-empty-p line)
        (elpaca--signal e (car (last (split-string line "" t)))
                        (and pattern (string-match-p pattern line) status) returnp)))
    (when (and pattern (string-match-p pattern output))
      (process-put process :result nil)
      (if (eq status 'failed)
          (elpaca--fail e output)
        (elpaca--signal e output status)))))

(defun elpaca--process-sentinel (&optional info status process event)
  "Update E's INFO and STATUS when PROCESS EVENT is finished."
  (if-let ((e (process-get process :elpaca))
           ((and (equal event "finished\n") (not (eq (elpaca--status e) 'failed)))))
      (progn (elpaca--signal e info status)
             (elpaca--continue-build e))
    (elpaca--fail e (process-get process :output))))

(defun elpaca--compile-info-process-sentinel (process event)
  "Sentinel for info compilation PROCESS EVENT."
  (let* ((e  (process-get process :elpaca))
         (finished (equal event "finished\n")))
    (elpaca--signal e (if finished
                          "Info compiled"
                        (format "Failed to compile Info: %S" (string-trim event))))
    (unless finished
      (setf (elpaca<-build-steps e)
            (cl-set-difference (elpaca<-build-steps e)
                               '(elpaca--install-info elpaca--add-info-path))))
    (elpaca--continue-build e)))

(defun elpaca--compile-info (e)
  "Compile E's .texi files."
  (elpaca--signal e "Compiling Info files" 'info)
  (if-let ((elpaca-makeinfo-executable)
           (files
            (cl-loop for (repo-file . build-file) in
                     (or (elpaca<-files e)
                         (setf (elpaca<-files e) (elpaca--files e)))
                     for f = (when-let (((string-match-p "\\.texi\\(nfo\\)?$" repo-file))
                                        (info (concat (file-name-sans-extension build-file) ".info"))
                                        ((not (file-exists-p info))))
                               (list repo-file "-o" info))
                     when f collect f))
           (command `(,elpaca-makeinfo-executable ,@(apply #'append files)))
           (process (make-process
                     :name (concat "elpaca-compile-info-" (elpaca<-package e))
                     :connection-type 'pipe
                     :command command
                     :filter   #'elpaca--process-filter
                     :sentinel #'elpaca--compile-info-process-sentinel)))
      (process-put process :elpaca e)
    (elpaca--signal
     e (if elpaca-makeinfo-executable "No .info files found" "makeinfo executable not found"))
    (elpaca--remove-build-steps e '(elpaca--install-info elpaca--add-info-path))
    (elpaca--continue-build e)))

(defun elpaca--install-info-process-sentinel (process event)
  "Sentinel for info installation PROCESS EVENT."
  (let ((e (process-get process :elpaca)))
    (elpaca--signal e (if (equal event "finished\n")
                          "Info installed"
                        (format "Failed to install Info: %S" (string-trim event))))
    (elpaca--continue-build e)))

(defun elpaca--install-info-async (file dir e)
  "Asynchronously Install E's .info FILE in Info DIR."
  (elpaca--signal e file)
  (let ((process (make-process
                  :name (concat "elpaca-install-info-" (elpaca<-package e))
                  :connection-type 'pipe
                  :command (list elpaca-install-info-executable file dir)
                  :filter #'elpaca--process-filter
                  :sentinel #'elpaca--install-info-process-sentinel)))
    (process-put process :elpaca e)))

(defun elpaca--install-info (e)
  "Install E's .info files."
  (when-let ((dir (expand-file-name "dir" (elpaca<-build-dir e)))
             ((not (file-exists-p dir)))
             (specs (or (elpaca<-files e) (setf (elpaca<-files e) (elpaca--files e)))))
    (elpaca--signal e "Installing Info files")
    (cl-loop for (target . link) in specs
             for file = (cond
                         ((string-match-p "\\.info$" link) link)
                         ((string-match-p "\\.texi\\(nfo\\)?$" target)
                          (concat (file-name-sans-extension link) ".info")))
             when (and file (file-exists-p file))
             do (setf (elpaca<-build-steps e)
                      (push (apply-partially #'elpaca--install-info-async file dir)
                            (elpaca<-build-steps e)))))
  (elpaca--continue-build e))

(defun elpaca--dispatch-build-commands-process-sentinel (process event)
  "PROCESS EVENT."
  (let ((e    (process-get process :elpaca))
        (type (process-get process :build-type)))
    (cond
     ((equal event "finished\n")
      (elpaca--signal e (concat type " steps finished"))
      (elpaca--continue-build e))
     ((string-match-p "abnormally" event)
      (elpaca--fail e (format "%s command failed" type))))))

(defun elpaca--dispatch-build-commands (e type)
  "Run E's TYPE commands for.
TYPE is either the keyword :pre-build, or :post-build.
Each command is either an elisp form to be evaluated or a list of
strings to be executed in a shell context of the form:

  (\"executable\" \"arg\"...)

Commands are exectued in the E's repository directory.
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
               (program           `(progn
                                     (setq load-prefer-newer t)
                                     (require 'elpaca)
                                     (normal-top-level-add-subdirs-to-load-path)
                                     (setq gc-cons-percentage 1.0) ;; trade memory for gc speed
                                     (elpaca--run-build-commands ',commands)))
               (process (make-process
                         :name (concat "elpaca-" name "-" (plist-get recipe :package))
                         :command (list
                                   emacs "-Q"
                                   "-L" "./"
                                   "-L" (expand-file-name "elpaca/" elpaca-builds-directory)
                                   "--batch"
                                   "--eval" (let (print-level print-length print-circle)
                                              (format "%S" program)))
                         :filter   #'elpaca--process-filter
                         :sentinel #'elpaca--dispatch-build-commands-process-sentinel)))
          (process-put process :elpaca e)
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

(defun elpaca--dependencies (e)
  "Return a list of E's declared dependencies."
  (let* ((default-directory (elpaca<-repo-dir e))
         (package (file-name-sans-extension (elpaca<-package e)))
         (name (concat package ".el"))
         (regexp (concat "^" name "$"))
         (main (or (plist-get (elpaca<-recipe e) :main)
                   (cl-some (lambda (f) (let ((e (expand-file-name f)))
                                          (and (file-exists-p e) e)))
                            (list (concat package "-pkg.el")
                                  name
                                  (concat "./lisp/" name)
                                  (concat "./elisp/" name)))
                   (car (directory-files default-directory nil regexp))
                   (car (elpaca--directory-files-recursively default-directory regexp))
                   ;; Best guess if there is no file matching the package name...
                   (car (directory-files default-directory nil "\\.el$" 'nosort))
                   (error "Unable to find main elisp file for %S" package)))
         (deps
          (if (not (file-exists-p default-directory))
              (error "Package repository not on disk: %S" (elpaca<-recipe e))
            (with-temp-buffer
              (insert-file-contents-literally main)
              (goto-char (point-min))
              (if (string-suffix-p "-pkg.el" main)
                  (eval (nth 4 (read (current-buffer))))
                (let ((case-fold-search t))
                  (when (re-search-forward "^;+[  ]+\\(Package-Requires\\)[   ]*:[  ]*"
                                           nil 'noerror)
                    (let ((deps (list (buffer-substring-no-properties (point) (line-end-position)))))
                      (forward-line 1)
                      (while (looking-at "^;+\\(\t\\|[\t\s]\\{2,\\}\\)\\(.+\\)")
                        (push (match-string-no-properties 2) deps)
                        (forward-line 1))
                      (condition-case err
                          (read (mapconcat #'identity (nreverse deps) " "))
                        ((error)
                         (error "%S Package-Requires error: %S" main err)))))))))))
    (cl-loop for dep in deps ; convert naked symbol or (symbol) to (symbol "0")
             collect (if (or (symbolp dep) (null (cdr-safe dep)))
                         (list (elpaca--first dep) "0")
                       dep))))

;;@DECOMPOSE: The body of this function is similar to `elpaca--clone-dependencies'.
;; Refactor into a macro to operate on dependencies?
(defun elpaca--queue-dependencies (e)
  "Queue E's dependencies."
  (elpaca--signal e "Queueing Dependencies" 'queueing-deps)
  (if-let ((queued (cl-loop
                    with queued = (elpaca--queued)
                    with e-id = (elpaca<-id e)
                    for (item . _) in (elpaca--dependencies e)
                    for d = (and (not (memq item elpaca-ignored-dependencies))
                                 (or (elpaca-alist-get item queued)
                                     (elpaca--queue item)))
                    when d collect
                    (prog1 d
                      (unless (memq item (elpaca<-dependencies e))
                        (push item (elpaca<-dependencies e)))
                      (unless (memq e-id (elpaca<-dependents d))
                        (push e-id (elpaca<-dependents d)))))))
      ;; We do this in two steps so that e is aware of all its
      ;; dependencies before any single dependency starts its build.
      ;; Otherwise a dependency may finish prior to other dependencies being
      ;; registered. This will cause the dependent e to become unblocked
      ;; multiple times and run its build steps simultaneously/out of order.
      (mapc #'elpaca--continue-build queued)
    (elpaca--signal e "No external dependencies detected")
    (elpaca--continue-build e)))

;;@TODO: fix possible race similar to queue--dependencies.
;;@MAYBE: Package major version checks.
(defun elpaca--clone-dependencies (e)
  "Clone E's dependencies."
  (elpaca--signal e "Cloning Dependencies" 'cloning-deps)
  (if-let ((dependencies (elpaca--dependencies e))
           (externals (cl-loop for dependency in dependencies
                               for item = (car dependency)
                               unless (memq item elpaca-ignored-dependencies)
                               collect item)))
      (if-let ((emacs (assoc 'emacs dependencies))
               ((version< emacs-version (cadr emacs))))
          (elpaca--fail e (format "Requires %S; running %S" emacs emacs-version))
        (when (= (length externals) ; Our dependencies beat us to the punch
                 (cl-loop with e-id = (elpaca<-id e)
                          for dependency in externals
                          for found = (elpaca-alist-get dependency (elpaca--queued))
                          for d = (or found (elpaca--queue dependency))
                          for d-id = (elpaca<-id d)
                          for includes = (elpaca<-includes e)
                          for included =
                          (member d-id (append includes (cl-loop for i in includes
                                                                 append (elpaca<-includes
                                                                         (elpaca-get-queued i)))))
                          for blocked = (eq (elpaca--status d) 'blocked)
                          do
                          (unless (memq d-id (elpaca<-dependencies e))
                            (push d-id (elpaca<-dependencies e)))
                          (unless (memq e-id (elpaca<-dependents d))
                            (push e-id (elpaca<-dependents d)))
                          (unless found
                            (if included
                                ;; Unblock dependency published in same repo...
                                (when blocked
                                  (elpaca--signal d nil 'unblocked-mono-repo)
                                  (elpaca--clone-dependencies d))
                              (unless blocked (elpaca--continue-build d))))
                          count (and found (eq (elpaca--status found) 'finished))))
          (elpaca--continue-build e)))
    (elpaca--signal e "No external dependencies detected")
    (elpaca--continue-build e)))

(defun elpaca--remote-default (remote)
  "Return REMOTE's \"default\" branch.
This is the branch that would be checked out upon cloning."
  (elpaca-with-process (elpaca-process-call "git" "remote" "show" remote)
    (if success
        (if (string-match "\\(?:[^z-a]*HEAD branch:[[:space:]]+\\([^z-a]*?\\)$\\)" stdout)
            (match-string 1 stdout)
          (error "Unable to determie remote default branch"))
      (error (format "Remote default branch error: %S" stderr)))))

(defun elpaca--checkout-ref (e)
  "Check out E's ref."
  (let* ((recipe            (elpaca<-recipe   e))
         (default-directory (elpaca<-repo-dir e))
         (remotes (plist-get recipe :remotes))
         (remote (when-let ((first (elpaca--first remotes)))
                   (if (stringp first) first
                     (setq recipe (elpaca-merge-plists recipe (cdr first)))
                     (car-safe first))))
         (ref (plist-get recipe :ref))
         (tag (plist-get recipe :tag))
         (branch (plist-get recipe :branch))
         (target (or ref tag branch)))
    (if (null target)
        (progn (if-let (((and remotes (listp remotes)))
                        (branch (condition-case err
                                    (elpaca--remote-default remote)
                                  (elpaca--fail e "Remote default branch err: %S" err)))
                        (name (concat remote "/" branch)))
                   (progn
                     (elpaca--call-with-log
                      e 1 "git" "branch" "-m"
                      (string-trim
                       (elpaca-process-output
                        "git" "rev-parse" "--abbrev-ref" "--symbolic-full-name" "@{u}")))
                     (elpaca--call-with-log e 1 "git" "checkout" "-b" branch "--track" name)
                     (elpaca--signal e (concat (elpaca--first remote) " HEAD checked out")
                                     'ref-checked-out))
                 (elpaca--signal e nil 'ref-checked-out))
               (elpaca--continue-build e))
      (cond
       ((and ref (or branch tag))
        (elpaca--signal
         e (format ":ref %S overriding %S %S" ref (if branch :branch :tag) (or branch tag))))
       ((and tag branch)
        (elpaca--fail e (format "Ambiguous ref: :tag %S, :branch %S" tag branch))))
      (elpaca--signal e (concat "Checking out " target))
      (let ((process
             (make-process
              :name (format "elpaca-checkout-ref-%s" (elpaca<-package e))
              :connection-type 'pipe
              :command
              `("git" "-c" "advice.detachedHead=false" ;ref, tag may detach HEAD
                ,@(cond
                   (ref    (list "checkout" ref))
                   (tag    (list "checkout" (concat ".git/refs/tags/" tag)))
                   (branch (list "switch" "-C" branch
                                 (concat (elpaca--first remote) "/" branch)))))
              :filter   #'elpaca--process-filter
              :sentinel (apply-partially #'elpaca--process-sentinel
                                         (concat target " checked out")
                                         'ref-checked-out))))
        (process-put process :elpaca e)))))

(defun elpaca--check-status (e)
  "Called when one of an E's dependencies change status.
Kick off next build step, and/or change E's status."
  (unless (eq (elpaca--status e) 'finished)
    (cl-loop with failed
             with blocked
             with queued = (elpaca--queued)
             for dependency in (elpaca<-dependencies e)
             for found = (elpaca-alist-get dependency queued)
             for status = (elpaca--status found)
             unless (eq status 'finished)
             do (push dependency (if (eq status 'failed) failed blocked))
             finally
             (cond
              (failed (elpaca--fail e (format "Failed dependencies: %S" failed)))
              (blocked (elpaca--signal
                        e (concat "Blocked by dependencies: " (prin1-to-string blocked)) 'blocked))
              (t (elpaca--signal e "unblocked by dependency" 'unblocked)
                 (elpaca--continue-build e))))))

(defun elpaca--clone-process-sentinel (process _event)
  "Sentinel for clone PROCESS."
  (let ((e   (process-get process :elpaca))
        (raw (process-get process :raw-output)))
    (if (and (string-match-p "fatal" raw) (not (string-match-p "already exists" raw)))
        (elpaca--fail e (nth 2 (car (elpaca<-log e))))
      (elpaca--continue-build e))))

(defun elpaca--clone (e)
  "Clone E's repo to `elpaca-directory'."
  (let* ((recipe  (elpaca<-recipe   e))
         (package (plist-get recipe :package))
         (depth   (plist-get recipe :depth))
         (repodir (elpaca<-repo-dir e))
         (URI     (elpaca--repo-uri recipe))
         (default-directory elpaca-directory)
         (command
          `("git" "clone"
            ;;@TODO: Some refs will need a full clone or specific branch.
            ,@(when (numberp depth)
                (if (plist-get recipe :ref)
                    (elpaca--signal e "ignoring :depth in favor of :ref")
                  (list "--depth" (number-to-string depth) "--no-single-branch")))
            ,@(when-let ((remotes (plist-get recipe :remotes))
                         ((listp remotes)))
                '("--no-checkout"))
            ,URI ,repodir)))
    (elpaca--signal e "Cloning" 'cloning)
    (let ((process
           (make-process
            :name     (concat "elpaca-clone-" package)
            :command  command
            :filter   (lambda (process output)
                        (elpaca--process-filter
                         process output
                         "\\(?:^\\(?:Password\\|Username\\|passphrase\\)\\)" 'blocked))
            :sentinel #'elpaca--clone-process-sentinel)))
      (process-put process :elpaca e)
      (setf (elpaca<-process e) process))))

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
  "Generate E's autoloads.
Async wrapper for `elpaca-generate-autoloads'."
  (let* ((package           (elpaca<-package  e))
         (build-dir         (elpaca<-build-dir e))
         (default-directory build-dir)
         (elpaca            (expand-file-name "repos/elpaca/" elpaca-directory))
         (program           `(progn (setq gc-cons-percentage 1.0)
                                    (elpaca-generate-autoloads ,package ,build-dir)))
         (command
          (list (elpaca--emacs-path) "-Q"
                "-L" elpaca
                "-L" build-dir ; Is this necessary?
                "-l" (expand-file-name "elpaca.el" elpaca)
                "--batch" "--eval" (let (print-level print-length print-circle)
                                     (format "%S" program))))
         (process
          (make-process
           :name     (concat "elpaca-autoloads-" package)
           :connection-type 'pipe
           :command  command
           :filter   #'elpaca--process-filter
           :sentinel (apply-partially #'elpaca--process-sentinel "Autoloads Generated" nil))))
    (process-put process :elpaca e)
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
    (cl-pushnew build-dir load-path)
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
                    (elpaca-q<-autoloads (car (last elpaca--queues (1+ (elpaca<-queue-id e)))))))
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
  (let* ((build-dir         (elpaca<-build-dir e))
         (default-directory build-dir)
         (emacs             (elpaca--emacs-path))
         (dependency-dirs
          (cl-loop for item in (elpaca-dependencies (elpaca<-id e)
                                                    elpaca-ignored-dependencies)
                   when item
                   for build-dir = (elpaca<-build-dir (elpaca-alist-get item (elpaca--queued)))
                   when build-dir collect build-dir))
         (program `(progn
                     (mapc (lambda (dir) (let ((default-directory dir))
                                           (add-to-list 'load-path dir)
                                           (normal-top-level-add-subdirs-to-load-path)))
                           ',(append dependency-dirs (list build-dir)))
                     (setq gc-cons-percentage 1.0) ;; trade memory for gc speed
                     (byte-recompile-directory ,build-dir 0 'force)))
         (print-level nil)
         (print-circle nil)
         (process
          (make-process
           :name     (concat "elpaca-byte-compile-" (elpaca<-package e))
           :connection-type 'pipe
           :command  `(,emacs "-Q" "--batch" "--eval" ,(format "%S" program))
           :filter   #'elpaca--process-filter
           :sentinel (apply-partially #'elpaca--process-sentinel "Byte compilation complete" nil))))
    (process-put process :elpaca e)))

;;;###autoload
(defun elpaca-dependencies (item &optional ignore interactive recurse)
  "Return recursive list of ITEM's dependencies.
IGNORE may be a list of symbols which are not included in the resulting list.
RECURSE is used to track recursive calls.
When INTERACTIVE is non-nil, message the list of dependencies."
  (interactive (list (elpaca--read-queued "Dependencies of: ") nil t))
  (if-let ((e (or (elpaca-alist-get item (elpaca--queued))
                  (unless (member item elpaca-ignored-dependencies)
                    (elpaca<-create item))))
           (dependencies (elpaca--dependencies e)))
      (let* ((transitives (cl-loop for dependency in dependencies
                                   for name = (car dependency)
                                   unless (memq name ignore) collect
                                   (cons name (elpaca-dependencies name ignore nil 'recurse))))
             (deps (delete-dups (flatten-tree transitives))))
        (if interactive (message "%s" deps) deps))
    (when recurse item)))

;;;###autoload
(defun elpaca-dependents (item &optional interactive recurse)
  "Return recursive list of packages which depend on ITEM.
RECURSE is used to keep track of recursive calls.
When INTERACTIVE is non-nil, message the list of dependents."
  (interactive (list (elpaca--read-queued "Dependents of: ") t))
  (if-let ((e (elpaca-alist-get item (elpaca--queued)))
           (dependents (elpaca<-dependents e)))
      (let* ((transitives
              (cl-loop for dependent in dependents
                       collect (cons dependent (elpaca-dependents dependent nil 'recurse))))
             (deps (delete-dups (nreverse (flatten-tree transitives)))))
        (if interactive (message "%s" deps) deps))
    (when recurse item)))

;;;; COMMANDS/MACROS
;;;###autoload
(defmacro elpaca (order &rest body)
  "Install ORDER, then execute BODY.
If ORDER is `nil`, defer BODY until orders have been processed."
  (declare (indent 1))
  (when (equal 'quote (car-safe order)) (setq order (cadr order)))
  `(progn
     ,@(when body (list `(push ',(cons (elpaca--first order) body)
                               (elpaca-q<-forms (car elpaca--queues)))))
     ,@(unless (null order) (list `(elpaca--queue
                                    ,(if (equal '\` (car-safe order))
                                         order
                                       (list 'quote order)))))))

;;;###autoload
(defmacro elpaca-use-package (order &rest body)
  "Execute BODY in `use-package' declaration after ORDER is finished.
If the :disabled keyword is present in body, the package is completely ignored.
This happens regardless of the value associated with :disabled.
The expansion is a string indicating the package has been disabled."
  (declare (indent 1))
  (if (memq :disabled body)
      (format "%S :disabled by elpaca-use-package" order)
    (let ((o order))
      (when-let ((ensure (cl-position :ensure body)))
        (setq o (if (null (nth (1+ ensure) body)) nil order)
              body (append (cl-subseq body 0 ensure)
                           (cl-subseq body (+ ensure 2)))))
      `(elpaca ,o (use-package
                    ,(if-let (((memq (car-safe order) '(quote \`)))
                              (feature (flatten-tree order)))
                         (cadr feature)
                       (elpaca--first order))
                    ,@body)))))

(defvar elpaca--try-package-history nil "History for `elpaca-try'.")
(declare-function elpaca-log--latest "elpaca-log")
;;;###autoload
(defun elpaca-try (order &optional interactive)
  "Try ORDER.
Install the repo/build files on disk.
Activate the corresponding package for the current session.
ORDER's package is not made available during subsequent sessions.
When INTERACTIVE is non-nil, immediately process ORDER, otherwise queue ORDER."
  (interactive (list
                (if (equal current-prefix-arg '(4))
                    (minibuffer-with-setup-hook #'backward-char
                      (read (read-string "elpaca-try: " "()" 'elpaca--try-package-history)))
                  (let ((recipe (elpaca-menu-item
                                 nil nil nil
                                 (lambda (candidate)
                                   (not (elpaca-alist-get (car candidate)
                                                          (elpaca--queued)))))))
                    (append (list (intern (plist-get recipe :package)))
                            recipe)))
                t))
  (if (not interactive)
      (elpaca--queue (elpaca--first order))
    (require 'elpaca-log)
    (elpaca-log--latest)
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

(defun elpaca--process (queued)
  "Process QUEUED elpaca."
  (let ((e (cdr queued)))
    (unless (memq (elpaca--status e) '(failed blocked finished)) (elpaca--continue-build e))))

(defun elpaca--process-queue (q)
  "Process elpacas in Q."
  (when-let ((pre (elpaca-q<-pre q))) (funcall pre))
  (if (and (not (elpaca-q<-elpacas q)) (elpaca-q<-forms q))
      (elpaca--finalize-queue q)
    (mapc #'elpaca--process (reverse (elpaca-q<-elpacas q)))))

;;;###autoload
(defun elpaca-process-queues (&optional filter)
  "Process the incomplete queues.
FILTER must be a unary function which accepts and returns a queue list."
  (setq debug-on-error elpaca--debug-init elpaca--debug-init nil)
  (if-let ((queues (if filter (funcall filter (reverse elpaca--queues))
                     (reverse elpaca--queues)))
           (incomplete (cl-find 'incomplete queues :key #'elpaca-q<-status)))
      (elpaca--process-queue incomplete)
    (message "No incomplete queues to process")))

(defun elpaca--on-disk-p (item)
  "Return t if ITEM has an associated E and a build or repo dir on disk."
  (when-let ((e (elpaca-alist-get item (elpaca--queued))))
    (or (file-exists-p (elpaca<-repo-dir e)) (file-exists-p (elpaca<-build-dir e)))))

;;@MAYBE: Should this delete user's declared package if it is a dependency?
;;@MAYBE: user option for deletion policy when repo is dirty.
;;;###autoload
(defun elpaca-delete (id &optional force deps ignored)
  "Remove a package with ID from item cache and disk.
If DEPS is non-nil (interactively with \\[universal-argument]) delete dependencies.
IGNORED dependencies are not deleted.
If FORCE is non-nil (interacitvley with \\[universal-argument] \\[universal-argument])
do not confirm before deleting package and DEPS."
  (interactive (list (elpaca--read-queued "Delete Package: ")
                     (equal current-prefix-arg '(16))
                     (and current-prefix-arg (listp current-prefix-arg))))
  (when (or force (yes-or-no-p (format "Delete package %S?" id)))
    (if-let ((e (elpaca-alist-get id (elpaca--queued))))
        (let* ((repo-dir      (elpaca<-repo-dir  e))
               (repo-p        (and repo-dir (file-exists-p repo-dir)))
               (build-dir     (elpaca<-build-dir e))
               (ignored       (if (listp ignored) ignored (list ignored)))
               (dependents    (cl-set-difference (elpaca-dependents id) ignored))
               (dependencies  (and deps repo-p
                                   (ignore-errors (elpaca-dependencies
                                                   id elpaca-ignored-dependencies))))
               (recipe        (elpaca<-recipe e))
               (url           (plist-get recipe :url))
               (repo          (plist-get recipe :repo))
               (host          (or (plist-get recipe :host) (plist-get recipe :fetcher)))
               (info          (intern (concat url repo (and host (format "%s" host))))))
          (if (cl-some #'elpaca--on-disk-p dependents)
              (message "Cannot delete %S unless dependents %S are deleted first"
                       id dependents)
            (when repo-p (delete-directory repo-dir 'recursive))
            (when (file-exists-p build-dir) (delete-directory build-dir 'recursive))
            (dolist (queue elpaca--queues)
              (setf (elpaca-q<-elpacas queue)
                    (cl-remove id (elpaca-q<-elpacas queue) :key #'car)))
            (when (and (bound-and-true-p elpaca-log-buffer)
                       (equal (buffer-name) elpaca-log-buffer))
              (elpaca-status))
            (setf (alist-get info elpaca--repo-dirs nil 'remove) nil)
            (message "Deleted package %S" id)
            (dolist (dependency (and deps dependencies))
              (elpaca-delete dependency 'force deps (push id ignored)))))
      (let ((recipe (elpaca-recipe id)))
        (unless recipe (user-error "No recipe for %S" id))
        (when-let ((r (elpaca-repo-dir  recipe))) (delete-directory r 'recursive))
        (when-let ((b (elpaca-build-dir recipe))) (delete-directory b 'recursive))))))

(defun elpaca--file-package (&optional file)
  "Return queued E if current buffer's FILE is part of a repo, nil otherwise."
  (when-let ((name (or file (buffer-file-name))))
    (cl-find-if (lambda (e)
                  (string-prefix-p (file-name-as-directory (elpaca<-repo-dir e))
                                   (file-name-directory name)))
                (elpaca--queued) :key #'cdr)))

(defun elpaca--read-queued (&optional prompt)
  "Return queued item.
If PROMPT is non-nil, it is used instead of the default."
  (intern (completing-read
           (or prompt "Queued item: ")
           (sort (cl-delete-duplicates (mapcar #'car (elpaca--queued))) #'string<)
           nil t)))

(defun elpaca--unprocess (e)
  "Mark E as unprocessed in its queue."
  (when-let ((id (elpaca<-queue-id e))
             (q  (nth id (reverse elpaca--queues))))
    (cl-decf (elpaca-q<-processed q))
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
  (let* ((queued (assoc item (elpaca--queued)))
         (e (cdr queued)))
    (unless e (user-error "Package %S is not queued" item))
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
    (elpaca--signal e "Rebuilding" 'rebuilding)
    (setq elpaca-cache-autoloads nil)
    (elpaca--unprocess e)
    (setf (elpaca<-queue-time e) (current-time))
    (setf (elpaca<-statuses e) '(queued))
    (setf (elpaca<-files e) nil)
    (when interactive
      (require 'elpaca-log) ;@TODO: make conditional
      (elpaca-log--latest)
      (elpaca--process queued))))

(defun elpaca--log-updates (e)
  "Log E's fetched commits."
  (elpaca--signal e nil 'update-log)
  (let* ((default-directory (elpaca<-repo-dir e))
         (process (make-process
                   :name (concat "elpaca-log-updates-" (elpaca<-package e))
                   :connection-type 'pipe
                   ;; Pager will break this process. Complains about terminal functionality.
                   :command (list "git" "--no-pager" "log" "..@{u}")
                   :filter   #'elpaca--process-filter
                   :sentinel (apply-partially #'elpaca--process-sentinel nil nil))))
    (process-put process :elpaca e)))

(defun elpaca--fetch (e)
  "Fetch E's remotes' commits."
  (elpaca--signal e nil 'fetching-remotes)
  (let* ((default-directory (elpaca<-repo-dir e))
         (process (make-process
                   :name (concat "elpaca-fetch-" (elpaca<-package e))
                   :command  '("git" "fetch" "--all" "-v") ;;@TODO: make --all optional
                   :filter   #'elpaca--process-filter
                   :sentinel (apply-partially #'elpaca--process-sentinel "Remotes fetched" nil))))
    (process-put process :elpaca e)))

;;;###autoload
(defun elpaca-fetch (item &optional interactive)
  "Fetch ITEM's associated package remote commits.
This does not merge changes or rebuild the packages.
If INTERACTIVE is non-nil immediately process, otherwise queue."
  (interactive (list (elpaca--read-queued "Fetch Package Updates: ") t))
  (if-let ((queued (assoc item (elpaca--queued))))
      (let ((e (cdr queued)))
        (elpaca--signal e "Fetching updates" 'fetching-updates)
        (setf (elpaca<-build-steps e) (list #'elpaca--fetch #'elpaca--log-updates))
        (setf (elpaca<-queue-time e) (current-time))
        (when interactive
          (require 'elpaca-log)
          (elpaca-log--latest)
          (elpaca--process queued)))
    (user-error "Package %S is not queued" item)))

;;;###autoload
(defun elpaca-fetch-all (&optional interactive)
  "Fetch remote commits for queued elpacas.
INTERACTIVE is passed to `elpaca-fetch'."
  (interactive (list t))
  (cl-loop for (item . _) in (cl-remove-duplicates (elpaca--queued) :key #'car)
           do (elpaca-fetch item interactive)))

(defun elpaca--merge-process-sentinel (process event)
  "Handle PROCESS EVENT."
  (when-let (((equal event "finished\n"))
             (e (process-get process :elpaca)))
    (if (string-match-p "Already up to date" (nth 2 (car (elpaca<-log e))))
        (elpaca--finalize e)
      (elpaca--signal e "Updates merged" 'merged)
      (elpaca--continue-build e))))

(defun elpaca--merge (e)
  "Merge E's fetched commits."
  (let* ((default-directory (elpaca<-repo-dir e))
         (process (make-process
                   :name (concat "elpaca-merge-" (elpaca<-package e))
                   :connection-type 'pipe
                   :command  '("git" "merge" "--ff-only")
                   :filter (lambda (process output)
                             (elpaca--process-filter process output "fatal" 'failed))
                   :sentinel #'elpaca--merge-process-sentinel)))
    (elpaca--signal e "Merging updates" 'merging)
    (process-put process :elpaca e)))

(defun elpaca--announce-pin (e)
  "Dummy build step to announce a E's package is pinned."
  (elpaca--signal e "Skipping pinned package" 'pinned)
  (elpaca--continue-build e))

;;;###autoload
(defun elpaca-update (item &optional interactive)
  "Update ITEM's associated package.
If INTERACTIVE is non-nil, the queued order is processed immediately."
  (interactive (list (elpaca--read-queued "Update package: ") t))
  (let* ((queued (assoc item (elpaca--queued)))
         (e (cdr queued))
         (recipe (elpaca<-recipe e))
         (pin (plist-get recipe :pin)))
    (unless queued (user-error "Package %S is not queued" item))
    (elpaca--signal e "Fetching updates" 'fetching-updates)
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
          (elpaca<-queue-time e) (current-time))
    (elpaca--unprocess e)
    (when interactive
      (require 'elpaca-log) ;@TODO: make conditional
      (elpaca-log--latest)
      (elpaca--process queued))))

;;;###autoload
(defun elpaca-update-all (&optional interactive)
  "Update all queued packages.
When INTERACTIVE, immediately process queue."
  (interactive (list t))
  (cl-loop with repos
           for (item . e) in (reverse (elpaca--queued))
           for repo = (elpaca<-repo-dir e)
           unless (member repo repos) ;skip mono-repos
           do (push repo repos)
           (elpaca-update item))
  (when interactive
    (require 'elpaca-log)
    (elpaca-log--latest)
    (elpaca-process-queues)))

;;; Lockfiles
(defun elpaca-declared-p (item)
  "Return t if ITEM is declared in user's init file, nil otherwise."
  (when-let ((e (elpaca-alist-get item (elpaca--queued))))
    (or (elpaca<-init e)
        (cl-loop for dependent in (elpaca-dependents item)
                 when (elpaca-declared-p dependent) return t))))

(defun elpaca-installed-p (item)
  "Return t if ITEM's associated repo directory is on disk, nil otherwise."
  (and-let* ((e (elpaca-alist-get item (elpaca--queued)))
             (repo-dir (elpaca<-repo-dir e))
             ((file-exists-p repo-dir)))))

(defun elpaca-worktree-dirty-p (item)
  "Return t if ITEM's associated repository has a dirty worktree, nil otherwise."
  (when-let ((e (elpaca-alist-get item (elpaca--queued)))
             (recipe (elpaca<-recipe e))
             (repo-dir (elpaca<-repo-dir e))
             ((file-exists-p repo-dir))
             (default-directory repo-dir))
    (not (string-empty-p (elpaca-process-output
                          "git" "-c" "status.branch=false" "status" "--short")))))

;;;###autoload
(defun elpaca-unshallow (item) ;;@TODO: make async?
  "Convert ITEM's repo to an unshallow repository."
  (interactive (list (elpaca--read-queued "Unshallow Package Repository: ")))
  (when-let ((e (or (alist-get item (elpaca--queued))
                    (user-error "%s is not queued" item)))
             (repo-dir (or (elpaca<-repo-dir e)
                           (user-error "%s has no associated repo dir" item)))
             (default-directory repo-dir)
             ((or (equal
                   (string-trim
                    (elpaca-process-output "git" "rev-parse" "--is-shallow-repository"))
                   "true")
                  (user-error "%s is not a shallwow repository" repo-dir)))
             (remotes (plist-get (elpaca<-recipe e) :remotes)))
    (cl-loop for remote in (if (stringp remotes) (list remotes) remotes)
             for name = (elpaca--first remote)
             do
             (progn
               (elpaca-process-call "git" "config" (format "remote.%s.fetch" name)
                                    (format "+refs/heads/*:refs/remotes/%s/*" name))
               (elpaca-process-call "git" "fetch" "--unshallow" name)))))

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
(defmacro elpaca-with-dir (type item &rest body)
  "Set `default-directory' for duration of BODY.
TYPE is either `:repo' or `:build' for ITEM's repo or build directory."
  (declare (indent 2) (debug t))
  `(let* ((e (or (alist-get ,item (elpaca--queued))
                 (user-error "Not a queued item: %S" item)))
          (default-directory
           (,(intern (format "elpaca<-%s-dir" (substring (symbol-name type) 1))) e)))
     ,@body))

(declare-function elpaca-ui-current-package "elpaca-ui")
;;;###autoload
(defun elpaca-visit (&optional item build)
  "Open ITEM's local repository directory.
When BUILD is non-nil visit ITEM's build directory."
  (interactive (list (or (ignore-errors (elpaca-ui-current-package))
                         (elpaca--read-queued
                          (format "Visit %s dir: " (if current-prefix-arg "build" "repo"))))
                     current-prefix-arg))
  (when (eq item '##) (setq item nil)) ; Empty `elpaca--read-queued' response
  (if (not item)
      (find-file (or (and build elpaca-builds-directory)
                     (expand-file-name "./repos/" elpaca-directory)))
    (if-let ((e (alist-get item (elpaca--queued)))
             (dir (if build (elpaca<-build-dir e) (elpaca<-repo-dir e))))
        (if (file-exists-p dir)
            (find-file dir)
          (user-error "Directory does not exist: %S" dir))
      (user-error "%S is not a queued package" item))))

;;;###autoload
(defun elpaca-browse (item)
  "Browse ITEM's :url."
  (interactive (list (car (elpaca-menu-item))))
  (if-let ((url (or (when-let ((found (elpaca-get-queued item))
                               (recipe (elpaca<-recipe found)))
                      (file-name-sans-extension
                       (elpaca--repo-uri (elpaca-merge-plists recipe '(:protocol https)))))
                    (when-let ((found (alist-get item (elpaca--menu-items))))
                      (plist-get found :url)))))
      (browse-url url)
    (user-error "No URL associated with current line")))

;;;###autoload
(defun elpaca-version (&optional message)
  "Return elpaca version information string.
If MESSAGE is non-nil, the information is messaged."
  (interactive '(t))
  (let* ((default-directory (expand-file-name "repos/elpaca/" elpaca-directory))
         (info (string-trim (elpaca-process-output "git" "log" "--pretty=%h %D" "-1"))))
    (if message (message "Elpaca version: %s\n%s" info (emacs-version)) info)))

(provide 'elpaca)
;;; elpaca.el ends here
