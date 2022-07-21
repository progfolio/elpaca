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
;; accessed via an faux "arrow" like syntax. e.g. elpaca<-repo-dir
;; The main data structures of this program are "elpacas" and "queues".
;; They are abbreviated as "e" and "q" when used as function parameters.

;;; Code:
(eval-and-compile (require 'cl-lib))
(eval-when-compile (require 'subr-x))
(cl-declaim (optimize (safety 0) (speed 3)))
(require 'text-property-search)
(require 'elpaca-process)
(declare-function autoload-rubric "autoload")
(declare-function info-initialize "info")
(declare-function url-filename    "url-parse")
(declare-function url-host        "url-parse")
(defvar autoload-timestamps)
(defvar generated-autoload-file)
(defvar Info-directory-list)

(defgroup elpaca nil
  "An elisp package manager."
  :group 'elpaca
  :prefix "elpaca-")

(defface elpaca-finished
  '((t (:weight bold :foreground "#00FF00")))
  "Indicates an order is finished.")

(defface elpaca-blocked
  '((t (:weight bold :foreground "#FFC1CC")))
  "Indicates an order is blocked.")

(defface elpaca-failed
  '((t (:weight bold :foreground "#FF1818")))
  "Indicates an order has failed.")

(defvar elpaca--info-timer nil "Timer to debounce order info printing.")
(defvar elpaca--pre-built-steps
  '(elpaca--queue-dependencies elpaca--add-info-path elpaca--activate-package)
  "List of steps for packages which are already built.")

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
Results in faster start-up time.
However, loading errors will prevent later package autoloads from loading."
  :type 'boolean)

(defcustom elpaca-cache-menu-items t
  "When non-nil, menu-items ares cached. Speeds up init load."
  :type 'boolean)

(defcustom elpaca-directory (expand-file-name "elpaca" user-emacs-directory)
  "Location of the elpaca package store."
  :type 'directory)

(defvar elpaca-cache-directory (expand-file-name "cache" elpaca-directory)
  "Location of the cache directory.")

(defcustom elpaca-makeinfo-executable (executable-find "makeinfo")
  "Path of the makeinfo executable."
  :type 'string)

(defcustom elpaca-install-info-executable
  (executable-find "install-info")
  "Path of the install-info executable."
  :type 'string)

(defcustom elpaca-info-timer-interval 0.02
  "Number of idle seconds to wait before printing order statuses.
Setting this to too low may cause the status buffer to block more.
Setting it too high causes prints fewer status updates."
  :type 'number)

(defcustom elpaca--process-busy-interval 5
  "Seconds to wait between subprocess outputs before declaring process blocked."
  :type 'number)

(defcustom elpaca-use-build-step-short-names t
  "When non-nil, recipe :build functions are auto-prefixed with `elpaca--`."
  :type 'boolean)

(defcustom elpaca-build-steps '(elpaca--clone
                                elpaca--add-remotes
                                elpaca--checkout-ref
                                elpaca--run-pre-build-commands
                                elpaca--clone-dependencies
                                elpaca--link-build-files
                                elpaca--byte-compile
                                elpaca--generate-autoloads-async
                                elpaca--compile-info
                                elpaca--install-info
                                elpaca--add-info-path
                                elpaca--run-post-build-commands
                                elpaca--activate-package)
  "List of steps which are run when installing/building a package."
  :type 'list)

(defvar elpaca-default-files-directive
  '("*.el" "*.el.in" "dir"
    "*.info" "*.texi" "*.texinfo"
    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el" "LICENSE"
              "README*"))
  "Default value for the `:files' directive in recipes.
It is also spliced in at any point where the `:defaults' keyword
is used in a `:files' directive.")

(defvar elpaca-order-defaults
  (list :protocol 'https :remotes "origin" :inherit t :depth 1)
  "Default order modifications.")
(defun elpaca-order-defaults (_order)
  "Matches any order."
  elpaca-order-defaults)

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

(defcustom elpaca-hide-status-during-build nil
  "When non-nil, don't display `elpaca-status' when a package requires a build."
  :type 'boolean)

(defvar elpaca--show-status (not elpaca-hide-status-during-build)
  "When non-nil, show `elpaca-status' during build.")

(defvar elpaca-ignored-dependencies
  '(emacs cl-lib cl-generic nadvice org org-mode map seq json)
  "Ignore these unless the user explicitly requests they be installed.")

(defvar elpaca-overriding-prompt nil "Overriding prompt for interactive functions.")

(defun elpaca--read-file (path)
  "Read file at PATH into memory."
  (when (file-exists-p path)
    (condition-case err
        (with-temp-buffer
          (insert-file-contents path)
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
              (print-circle nil)
              (print-level  nil)
              (print-length nil))
         ,@body
         nil))))

(defun elpaca--read-menu-cache ()
  "Read the menu-cache."
  (elpaca--read-file (expand-file-name "menu-items.eld" elpaca-cache-directory)))

(defvar elpaca--menu-items-cache
  (when elpaca-cache-menu-items (elpaca--read-menu-cache))
  "Cache for menu candidates.")

(defvar elpaca--package-requires-regexp
  "\\(?:[[:space:]]*;+[[:space:]]*Package-Requires:[[:space:]]*\\(([^z-a]*?))\\)\\)"
  "Regexp matching the Package-Requires metadata in an elisp source file.")

(defvar elpaca--queues nil
  "List of elpaca queue objects.")

(cl-defstruct (elpaca-q< (:constructor elpaca-q<-create)
                         (:type list)
                         (:copier nil)
                         (:named))
  "Queue to hold elpacas."
  (type (unless after-init-time 'init))
  (id   (length elpaca--queues))
  (processed 0)
  (status 'incomplete)
  (time (current-time))
  autoloads forms elpacas)

;;@TODO: We should fix this so that the list is initialized in its defvar
;; and properly set up before adding to it.
(unless elpaca--queues (setq elpaca--queues (list (elpaca-q<-create))))

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

;;@TODO:
;;- allow passing in menu functions.
(defun elpaca--menu-items (&optional recache)
  "Return alist of `elpaca-menu-functions' candidates.
If RECACHE is non-nil, recompute `elpaca--menu-items-cache'."
  (or (and (not recache) elpaca-cache-menu-items elpaca--menu-items-cache)
      (prog1
          (setq elpaca--menu-items-cache
                (sort (copy-tree
                       (cl-loop for fn in elpaca-menu-functions
                                ;; Allows adding a symbol prior menu installation.
                                append (and (functionp fn) (funcall fn 'index))))
                      (lambda (a b) (string-lessp (car a) (car b)))))
        (when elpaca-cache-menu-items (elpaca--write-menu-cache)))))

(defsubst elpaca-alist-get (key alist)
  "Return KEY's value in ALIST.
Simplified version of `alist-get'."
  (cdr (assq key alist)))

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
  (let* ((menus (if (eq interactive '(4))
                    (mapcar #'intern-soft
                            (cl-remove-duplicates
                             (completing-read-multiple "Menus: " elpaca-menu-functions
                                                       nil t)
                             :test #'equal))
                  (or menus elpaca-menu-functions (user-error "No menus found"))))
         (elpaca-menu-functions menus)
         (candidates
          (let ((c (if filter
                       (cl-remove-if-not filter (elpaca--menu-items))
                     (elpaca--menu-items))))
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
(defun elpaca-update-menus (&optional sources)
  "Update all menus in SOURCES or `elpaca-menu-functions'."
  (interactive (list (mapcar #'intern
                             (completing-read-multiple
                              "Update Menus: " elpaca-menu-functions))))
  (let ((elpaca-menu-functions (or sources elpaca-menu-functions)))
    (run-hook-with-args 'elpaca-menu-functions 'update))
  (elpaca--menu-items 'recache))

(defsubst elpaca--inheritance-disabled-p (obj)
  "Return t if OBJ explicitly has :inherit nil key val, nil otherwise."
  (when-let (((listp obj))
             (member (plist-member obj :inherit)))
    (not (cadr member))))

;;;###autoload
(defun elpaca-recipe (order)
  "Return recipe computed from ORDER.
ORDER is any of the following values:
  - nil. The order is prompted for.
  - an item symbol which will be looked up via `elpaca-menu-functions'
  - an order list of the form: //='(ITEM . PROPS)."
  (interactive (list (if-let ((elpaca-overriding-prompt "Recipe: ")
                              (recipe (elpaca-menu-item)))
                         (push (intern (plist-get recipe :package)) recipe)
                       (user-error "No recipe selected"))))
  (let* ((interactive (called-interactively-p 'interactive))
         (props (cdr-safe order))
         (item (if (listp order) (car order) order))
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
      (message "%S recipe copied to kill-ring:\n%S"
               (plist-get recipe :package) recipe))))

(defsubst elpaca--emacs-path ()
  "Return path to running Emacs."
  (concat invocation-directory invocation-name))

(defsubst elpaca--repo-name (string)
  "Return repo name portion of STRING."
  (substring string (1+ (string-match-p "/" string))))

(defsubst elpaca--repo-user (string)
  "Return user name portion of STRING."
  (substring string 0 (string-match-p "/" string)))

(defun elpaca--full-repo-protocol-p (string)
  "Return t if STRING specifies a protocol."
  ;;@TODO: this needs to be more robust.
  (and (string-match-p ":" string) t))

(defvar elpaca--repo-dirs nil "List of registered repository directories.")
(defun elpaca-repo-dir (recipe)
  "Return path to repo given RECIPE."
  (let* ((local-repo (plist-get recipe :local-repo))
         (url (plist-get recipe :url))
         (repo (plist-get recipe :repo))
         (host (or (plist-get recipe :host) (plist-get recipe :fetcher)))
         (user nil)
         (info (intern (concat url repo (symbol-name host))))
         (mono-repo (alist-get info elpaca--repo-dirs))
         (name (cond
                (local-repo
                 (if mono-repo (error "Duplicate :local-repo %S" local-repo) local-repo))
                (mono-repo mono-repo)
                (url
                 (unless (featurep 'url-parse) (require 'url-parse))
                 (file-name-base (directory-file-name (url-filename
                                                       (url-generic-parse-url url)))))
                (repo
                 (setq user (elpaca--repo-user repo))
                 (elpaca--repo-name repo))))
         (dir (if (and (not mono-repo)
                       (member name (mapcar #'cdr elpaca--repo-dirs)))
                  (string-join (list name (format "%s" host) user) ".")
                (and name (file-name-sans-extension name)))))
    (push (cons info dir) elpaca--repo-dirs)
    (expand-file-name (concat "repos/" dir) elpaca-directory)))

(defun elpaca-build-dir (recipe)
  "Return RECIPE's build dir."
  (expand-file-name (plist-get recipe :package)
                    (expand-file-name "builds/" elpaca-directory)))

(defun elpaca--repo-uri (recipe)
  "Return repo URI from RECIPE."
  (cl-destructuring-bind (&key (protocol 'https)
                               url
                               fetcher
                               (host fetcher)
                               (repo url) &allow-other-keys)
      recipe
    (if (elpaca--full-repo-protocol-p repo)
        repo
      (let ((protocol (pcase protocol
                        ('https '("https://" . "/"))
                        ('ssh   '("git@" . ":"))
                        (_      (signal 'wrong-type-argument `((https ssh) ,protocol)))))
            (host     (pcase host
                        ('github       "github.com")
                        ('gitlab       "gitlab.com")
                        ((pred stringp) host)
                        (_              (signal 'wrong-type-argument
                                                `(:host (github gitlab stringp) ,host ,recipe))))))
        (format "%s%s%s%s.git" (car protocol) host (cdr protocol) repo)))))

(defsubst elpaca--first (obj)
  "Return `car' of OBJ if it is a list, else OBJ."
  (if (listp obj) (car obj) obj))

(defun elpaca--build-steps1 (item)
  "Return a list of build functions for ITEM."
  (let* ((p (alist-get item (elpaca--queued)))
         (recipe (or (and p (elpaca<-recipe p))
                     (elpaca-recipe item)))
         (build (plist-member recipe :build))
         (steps (cadr build))
         (removep (and (eq (car-safe steps) :not) (pop steps))))
    (when (and elpaca-use-build-step-short-names (listp steps))
      (setq steps (cl-loop for step in steps
                           collect (intern (concat "elpaca--" (symbol-name step))))))
    (cond
     ((or (not build) (eq steps t)) elpaca-build-steps)
     (removep (cl-set-difference elpaca-build-steps steps))
     ((listp steps) steps))))

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
  "TRY to set arg. If error, fail P with INFO."
  (declare (indent 1) (debug t))
  `(condition-case err ,try
     ((error) (setq status 'failed info (format ,info err)) nil)))

(defsubst elpaca--mono-repo (repo-dir)
  "Return previously queued P with REPO-DIR."
  (cl-some (lambda (queued)
             (and-let* ((p (cdr queued))
                        ((equal repo-dir (elpaca<-repo-dir p)))
                        p)))
           (elpaca--queued)))

(defsubst elpaca--build-steps (item builtp clonedp mono-repo)
  "Return list of build functions for ITEM.
BUILTP, CLONEDP, and MONO-REPO control which steps are excluded."
  (if builtp
      elpaca--pre-built-steps
    (unless elpaca-hide-status-during-build (setq elpaca--show-status t))
    (when-let ((steps (elpaca--build-steps1 item)))
      (when (and mono-repo (memq 'ref-checked-out (elpaca<-statuses mono-repo)))
        (setq steps (cl-set-difference steps
                                       '(elpaca--clone elpaca--add-remotes elpaca--checkout-ref))))
      (when clonedp (setq steps (delq 'elpaca--clone steps)))
      steps)))

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
                                   (p (elpaca--mono-repo repo-dir)))
                          (setq status 'blocked info (format "Waiting on monorepo %S" repo-dir))
                          p)))
         (build-steps (elpaca--build-steps item builtp clonedp mono-repo))
         (elpaca (elpaca<--create
                  :id id :package (format "%S" id) :item item :statuses (list status)
                  :repo-dir repo-dir :build-dir build-dir :mono-repo mono-repo
                  :files files :build-steps build-steps :recipe recipe
                  :includes (and mono-repo (list mono-repo))
                  :log (list (list status nil info)))))
    (when mono-repo (cl-pushnew elpaca (elpaca<-includes mono-repo)))
    elpaca))

(defun elpaca--fail (p &optional reason)
  "Fail P for REASON."
  (let ((item (elpaca<-item p))
        (queue (car (last elpaca--queues (1+ (elpaca<-queue-id p))))))
    (setf (elpaca-q<-forms queue)
          (assq-delete-all (elpaca--first item) (elpaca-q<-forms queue))))
  (elpaca--update-info p reason 'failed)
  (elpaca--finalize p))

(defsubst elpaca--status (e)
  "Return `car' of E's statuses."
  (car (elpaca<-statuses e)))

(defun elpaca--log-event (e text &optional replace)
  "Store TEXT in E's log.
Each event is of the form: (STATUS TIME TEXT)
If REPLACE is non-nil, the most recent log entry is replaced."
  (let ((event (list (elpaca--status e) (current-time) text)))
    (if replace
        (setf (car (elpaca<-log e)) event)
      (push event (elpaca<-log e)))))

(defun elpaca--queued (&optional n)
  "Return list of elpacas from Nth queue.
If N is nil return a list of all queued elpacas."
  (nreverse
   (if n
       (copy-sequence (elpaca-q<-elpacas (nth n (reverse elpaca--queues))))
     (cl-loop for queue in elpaca--queues append (elpaca-q<-elpacas queue)))))

;;@TODO: make an alist?
(defsubst elpaca--status-face (status &optional default)
  "Return face for STATUS or DEFAULT if not found."
  (cond
   ((eq status 'blocked)  'elpaca-blocked)
   ((eq status 'finished) 'elpaca-finished)
   ((eq status 'failed)   'elpaca-failed)
   (t                     (or default 'default))))

(defun elpaca--run-build-commands (commands)
  "Run build COMMANDS."
  (dolist (command (if (listp (car commands)) commands (list commands)))
    (if (cl-every #'stringp command)
        (elpaca-with-process (apply #'elpaca-process-call command)
          (if success
              (message stdout)
            (message "Build command error: %S" result)
            (error "Build command failed: %S" stderr)))
      (eval command t))))

(defsubst elpaca--info (e)
  "Return E's most recent log event info."
  (nth 2 (car (elpaca<-log e))))

(defsubst elpaca--continue-build (e)
  "Run E's next build step with ARGS."
  (funcall (or (pop (elpaca<-build-steps e)) #'elpaca--finalize) e))

(defun elpaca--continue-mono-repo-dependency (e)
  "Continue processing E after its mono-repo is in the proper state."
  (unless (memq (elpaca<-statuses e) '(finished build-linked))
    (elpaca--remove-build-steps e '(elpaca--clone elpaca--add-remotes elpaca--checkout-ref))
    (elpaca--continue-build e)))

(defvar elpaca-log-buffer)
(declare-function elpaca-log "elpaca-log")
(declare-function elpaca-status "elpaca-status")
(declare-function elpaca-ui--update-search-filter "elpaca-ui")
(defun elpaca--update-info-buffers ()
  "Update views in `elpaca-log-buffer'."
  (when (and (boundp 'elpaca-log-buffer)
             (get-buffer-window elpaca-log-buffer t)) ;; log buffer visible
    (elpaca-ui--update-search-filter elpaca-log-buffer)))

(defun elpaca--update-info (e info &optional status replace)
  "Update E's INFO.
Print the elpaca status line in `elpaca-log-buffer'.
If STATUS is non-nil and differs from E's current STATUS,
signal E's depedentents to check (and possibly change) their statuses.
If REPLACE is non-nil, E's log is updated instead of appended."
  (when (and status (not (equal status (elpaca--status e))))
    (push status (elpaca<-statuses e))
    (when (memq status '(finished failed blocked))
      (mapc #'elpaca--check-status (elpaca<-dependents e)))
    (when (eq status 'ref-checked-out)
      (mapc #'elpaca--continue-mono-repo-dependency (elpaca<-includes e)))
    (when (eq status 'failed) (elpaca-status)))
  (when info (elpaca--log-event e info replace))
  (when elpaca--info-timer (cancel-timer elpaca--info-timer))
  (setq elpaca--info-timer
        (run-at-time elpaca-info-timer-interval nil #'elpaca--update-info-buffers)))

(defun elpaca--log-duration (e)
  "Return E's log duration."
  (let* ((log (elpaca<-log e))
         (end (nth 1 (car log))))
    (time-subtract end (elpaca<-queue-time e))))

;;;###autoload
(defun elpaca-split-queue ()
  "Split remaining elpacas into new queue. Reuse current queue if it is empty."
  (when (elpaca-q<-elpacas (car elpaca--queues)) (push (elpaca-q<-create) elpaca--queues)))

;;;###autoload
(defmacro elpaca-queue (&rest body)
  "Execute BODY in its own queue."
  (declare (debug t))
  `(progn
     (elpaca-split-queue)
     ,@body
     (elpaca-split-queue)))

(defvar elpaca--finalize-queue-hook nil
  "Private hook run after a queue has been finalized.")

(defun elpaca--finalize-queue (q)
  "Run Q's post isntallation functions:
- load cached autoloads
- evaluate deferred package configuration forms
- possibly run `elpaca-after-init-hook'."
  (when-let ((autoloads (elpaca-q<-autoloads q)))
    (eval `(progn ,@autoloads) t))
  (when-let ((forms (elpaca-q<-forms q)))
    (eval `(progn ,@(apply #'append (mapcar #'cdr (reverse forms)))) t))
  (setf (elpaca-q<-status q) 'complete)
  (let ((next (nth (1+ (elpaca-q<-id q)) (reverse elpaca--queues))))
    (if (and (eq (elpaca-q<-type q) 'init)
             (or (null next)
                 (not (eq (elpaca-q<-type next) 'init))))
        (progn
          (run-hooks 'elpaca-after-init-hook)
          (elpaca-split-queue))
      (run-hooks 'elpaca--finalize-queue-hook)
      (run-hooks 'elpaca-post-queue-hook)
      (when next (elpaca--process-queue next)))))

(defun elpaca--finalize (e)
  "Declare E finished or failed."
  (let ((status (elpaca--status e)))
    (if (eq  status 'finished)
        (cl-loop for dependent in (elpaca<-dependents e)
                 unless (eq (elpaca--status dependent) 'finished)
                 do (elpaca--check-status dependent))
      (unless (eq (elpaca--status e) 'failed)
        (elpaca--update-info e
                             (concat  "✓ " (format-time-string "%s.%3N" (elpaca--log-duration e)) " secs")
                             'finished))
      (when-let ((q (car (last elpaca--queues (1+ (elpaca<-queue-id e)))))
                 ((= (cl-incf (elpaca-q<-processed q))
                     (length (elpaca-q<-elpacas q)))))
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
      (if (eq status 'failed)
          (elpaca--fail e info)
        (elpaca--update-info e info status))
      e)))

(defun elpaca--add-remotes (e &optional recurse)
  "Add E's repo remotes.
RECURSE is used to keep track of recursive calls."
  (let ((default-directory (elpaca<-repo-dir e))
        (recipe            (elpaca<-recipe   e)))
    (cl-destructuring-bind
        ( &key remotes
          ((:host recipe-host))
          ((:protocol recipe-protocol))
          ((:repo recipe-repo))
          &allow-other-keys)
        recipe
      (unless recurse (elpaca--update-info e "Adding Remotes"))
      (pcase remotes
        ("origin" nil)
        ((and (pred stringp) remote)
         (elpaca-process-call "git" "remote" "rename" "origin" remote))
        ((pred listp)
         (dolist (spec remotes)
           (if (stringp spec)
               (elpaca--add-remotes (let ((copy (copy-elpaca< e)))
                                      (setf (elpaca<-recipe copy)
                                            (plist-put (copy-tree recipe) :remotes spec))
                                      copy)
                                    'recurse)
             (pcase-let ((`(,remote . ,props) spec))
               (when props
                 (cl-destructuring-bind
                     (&key (host     recipe-host)
                           (protocol recipe-protocol)
                           (repo     recipe-repo)
                           &allow-other-keys
                           &aux
                           (recipe (list :host host :protocol protocol :repo repo)))
                     props
                   (elpaca-process-call
                    "git" "remote" "add" remote (elpaca--repo-uri recipe))
                   (unless (equal remote "origin")
                     (elpaca-process-call "git" "remote" "rename" "origin" remote))))))))
        (_ (elpaca--fail e (format "(wrong-type-argument ((stringp listp)) %S" remotes))))))
  (unless recurse (elpaca--continue-build e)))

(defun elpaca--remove-build-steps (p spec)
  "Remove each step in SPEC from P."
  (setf (elpaca<-build-steps p) (cl-set-difference (elpaca<-build-steps p) spec)))

(defun elpaca--files (p &optional files nocons)
  "Return alist of P :files to be symlinked: (PATH . TARGET PATH).
FILES and NOCONS are used recursively."
  (let* ((repo-dir          (elpaca<-repo-dir p))
         (default-directory repo-dir)
         (build-dir         (elpaca<-build-dir p))
         (recipe            (elpaca<-recipe p))
         (files             (or files (plist-get recipe :files)))
         (exclusions        nil)
         (targets           nil)
         (with-subdirs      nil))
    (dolist (el files)
      (pcase el
        ((pred stringp) (push (or (file-expand-wildcards el) el) targets))
        (`(:exclude  . ,excluded)
         (push (elpaca--files p excluded 'nocons) exclusions)
         nil)
        (:defaults
         (push (elpaca--files p elpaca-default-files-directive 'nocons) targets))
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
  (elpaca--update-info e "Linking build files")
  (let* ((build-dir (elpaca<-build-dir e))
         (files (or (elpaca<-files e)
                    (setf (elpaca<-files e) (elpaca--files e)))))
    (when (file-exists-p build-dir) (delete-directory build-dir 'recusrive))
    (make-directory build-dir 'parents)
    (dolist (spec files)
      (let ((file   (car spec))
            (link   (cdr spec)))
        (make-directory (file-name-directory link) 'parents)
        (make-symbolic-link file link 'overwrite))))
  (elpaca--update-info e "Build files linked" 'build-linked)
  (elpaca--continue-build e))

(defun elpaca--add-info-path (e)
  "Add the E's info to `Info-directory-list'."
  (let ((build-dir (elpaca<-build-dir e)))
    (if (file-exists-p (expand-file-name "dir" build-dir))
        (progn
          (elpaca--update-info e "Adding Info path" 'info)
          (with-eval-after-load 'info
            (info-initialize)
            (cl-pushnew build-dir Info-directory-list)))
      (elpaca--update-info e "No Info dir file found" 'info))
    (elpaca--continue-build e)))

(defun elpaca--process-busy (process)
  "Update E's status when PROCESS has stopped producing output."
  (when-let (((eq (process-status process) 'run))
             (e (process-get process :elpaca)))
    (elpaca--update-info e (process-get process :result) 'blocked)))

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
    (when timer (cancel-timer timer))
    (process-put process :timer (run-at-time elpaca--process-busy-interval nil
                                             (lambda () (elpaca--process-busy process))))
    (unless linep
      (process-put process :result (car (last lines)))
      (setq lines (butlast lines)))
    (dolist (line lines)
      (unless (string-empty-p line)
        (elpaca--update-info
         e (car (last (split-string line "" t)))
         (and pattern (string-match-p pattern line) status) returnp)))
    (when (and pattern (string-match-p pattern output))
      (process-put process :result nil)
      (if (eq status 'failed)
          (elpaca--fail e output)
        (elpaca--update-info e output status)))))

(defun elpaca--compile-info-process-sentinel (process event)
  "Sentinel for info compilation PROCESS EVENT."
  (let ((p  (process-get process :elpaca)))
    (elpaca--update-info p (if (equal event "finished\n")
                               "Info compiled"
                             (format "Failed to compile Info: %S" (string-trim event))))
    (elpaca--continue-build p)))

(defun elpaca--compile-info (e)
  "Compile E's .texi files."
  (elpaca--update-info e "Compiling Info files" 'info)
  (if-let ((files
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
                     :name (format "elpaca-compile-info-%s" (elpaca<-package e))
                     :command command
                     :filter   #'elpaca--process-filter
                     :sentinel #'elpaca--compile-info-process-sentinel)))
      (process-put process :elpaca e)
    (elpaca--update-info e "No .info files found")
    (elpaca--remove-build-steps e '(elpaca--install-info elpaca--add-info-path))
    (elpaca--continue-build e)))

;;@TODO: make async
(defun elpaca--install-info (e)
  "Install E's info files."
  (elpaca--update-info e "Installing Info files")
  (when-let ((dir (expand-file-name "dir" (elpaca<-build-dir e)))
             ((not (file-exists-p dir))))
    (cl-loop for (repo-file . build-file) in (or (elpaca<-files e)
                                                 (setf (elpaca<-files e) (elpaca--files e)))
             for f = (cond
                      ((string-match-p "\\.info$" build-file) build-file)
                      ((string-match-p "\\.texi\\(nfo\\)?$" repo-file)
                       (concat (file-name-sans-extension build-file) ".info")))
             when (and f (file-exists-p f))
             do (elpaca-with-process
                    (elpaca-process-call elpaca-install-info-executable f dir)
                  (unless success (elpaca--update-info e result)))))
  (elpaca--continue-build e))

(defun elpaca--dispatch-build-commands-process-sentinel (process event)
  "PROCESS EVENT."
  (let ((p    (process-get process :elpaca))
        (type (process-get process :build-type)))
    (cond
     ((equal event "finished\n")
      (elpaca--update-info
       p (format "%s steps finished" type) (intern (substring (symbol-name type) 1)))
      (elpaca--continue-build p))
     ((string-match-p "abnormally" event)
      ;; We want the event prior to the last "exited abnormally" event.
      (elpaca--fail p (nth 2 (car (last (elpaca<-log p) 2))))))))

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
           (commands (plist-get recipe type)))
      (progn
        (elpaca--update-info e (format "Running %S commands" type))
        (let* ((default-directory (elpaca<-repo-dir e))
               (emacs             (elpaca--emacs-path))
               (program           `(progn
                                     (require 'elpaca)
                                     (normal-top-level-add-subdirs-to-load-path)
                                     (elpaca--run-build-commands ',commands)))
               (process (make-process
                         :name (format "elpaca-%s-%s" type (plist-get recipe :package))
                         :command (list
                                   emacs "-Q"
                                   "-L" "./"
                                   "-L" (expand-file-name "repos/elpaca/" elpaca-directory)
                                   "--batch"
                                   "--eval" (let (print-level print-length)
                                              (format "%S" program)))
                         :filter   #'elpaca--process-filter
                         :sentinel #'elpaca--dispatch-build-commands-process-sentinel)))
          (process-put process :elpaca e)
          (process-put process :build-type type)))
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
  "Return a list of E's dependencies."
  (or (mapcar (lambda (o) (cons (cadr o) nil)) (elpaca<-dependencies e))
      (let* ((default-directory (elpaca<-repo-dir e))
             (package (file-name-sans-extension (elpaca<-package e)))
             (name (concat package ".el"))
             (regexp (concat "^" name "$"))
             (main (or
                    (plist-get (elpaca<-recipe e) :main)
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
                    (error "Unable to find main elisp file for %S" package))))
        (unless (file-exists-p default-directory)
          (error "Package repository not on disk: %S" (elpaca<-recipe e)))
        (with-temp-buffer
          (insert-file-contents-literally main)
          (goto-char (point-min))
          (if (string-suffix-p "-pkg.el" main)
              (eval (nth 4 (read (current-buffer))))
            (let ((case-fold-search t))
              (when (re-search-forward elpaca--package-requires-regexp nil 'noerror)
                (condition-case err
                    ;; Replace comment delimiters in multi-line package-requires metadata.
                    (read (replace-regexp-in-string ";" "" (match-string 1)))
                  ((error)
                   (error "Unable to parse %S Package-Requires metadata: %S" main err))))))))))

;;@DECOMPOSE: The body of this function is similar to `elpaca--clone-dependencies'.
;; Refactor into a macro to operate on dependencies?
(defun elpaca--queue-dependencies (e)
  "Queue E's dependencies."
  (elpaca--update-info e "Queueing Dependencies" 'queueing-deps)
  (let* ((dependencies (or (elpaca<-dependencies e) (elpaca--dependencies e)))
         (queued       (elpaca--queued))
         (queued-deps
          (cl-loop for (dependency . _) in dependencies
                   unless (memq dependency elpaca-ignored-dependencies)
                   for d = (or (elpaca-alist-get dependency queued)
                               (elpaca--queue dependency))
                   when d collect
                   (progn
                     (cl-pushnew d (elpaca<-dependencies e))
                     (cl-pushnew e (elpaca<-dependents d))
                     d))))
    (if queued-deps
        ;; We do this in two steps so that e is aware of all its
        ;; dependencies before any single dependency starts its build.
        ;; Otherwise a dependency may finish prior to other dependencies being
        ;; registered. This will cause the dependent e to become unblocked
        ;; multiple times and run its build steps simultaneously/out of order.
        (mapc #'elpaca--continue-build queued-deps)
      (elpaca--update-info e "No external dependencies detected")
      (elpaca--continue-build e))))

;;@TODO: fix possible race similar to queue--dependencies.
(defun elpaca--clone-dependencies (e)
  "Clone E's dependencies."
  (elpaca--update-info e "Cloning Dependencies" 'cloning-deps)
  (let* ((dependencies (elpaca--dependencies e))
         (externals (cl-loop with seen
                             for dependency in dependencies
                             for item = (car dependency)
                             unless (or (memq item elpaca-ignored-dependencies)
                                        (memq item seen))
                             collect dependency
                             do (push item seen))))
    (if-let ((emacs (assoc 'emacs dependencies))
             ((version< emacs-version (cadr emacs))))
        (elpaca--fail e (format "Requires %S; running %S" emacs emacs-version))
      (if externals
          ;;@TODO: Major Version conflict checks?
          (let ((finished 0))
            (dolist (spec externals)
              (let* ((dependency (car spec))
                     (queued     (elpaca-alist-get dependency (elpaca--queued)))
                     (d          (or queued (elpaca--queue dependency)))
                     (included   (member d (elpaca<-includes e)))
                     (blocked    (eq (elpaca--status d) 'blocked)))
                (cl-pushnew d (elpaca<-dependencies e))
                (cl-pushnew e (elpaca<-dependents d))
                (if queued
                    (when (eq (elpaca--status queued) 'finished) (cl-incf finished))
                  (if included
                      ;; Unblock dependency published in same repo...
                      (when blocked (elpaca--clone-dependencies d))
                    (unless blocked (elpaca--continue-build d))))))
            (when (= (length externals) finished) ; Our dependencies beat us to the punch
              (elpaca--continue-build e)))
        (elpaca--update-info e "No external dependencies detected")
        (elpaca--continue-build e)))))

(defun elpaca--checkout-ref-process-sentinel (process event)
  "PROCESS EVENT."
  (when-let (((equal event "finished\n"))
             (p                 (process-get process :elpaca))
             (recipe            (elpaca<-recipe   p))
             (default-directory (elpaca<-repo-dir p)))
    (let* ((remotes (plist-get recipe :remotes))
           (remote (elpaca--first remotes))
           (ref (plist-get recipe :ref))
           (tag (plist-get recipe :tag))
           (branch (or (plist-get (cdr-safe (car-safe remotes)) :branch)
                       (plist-get recipe :branch)))
           (target (or ref tag branch)))
      (when target
        (elpaca-with-process
            (apply #'elpaca-process-call
                   `("git"
                     ,@(cond
                        (ref    (list "checkout" ref))
                        (tag    (list "checkout" (concat ".git/refs/tags/" tag)))
                        (branch (list "switch" "-C" branch
                                      (format "%s/%s" (elpaca--first remote) branch))))))
          (unless success
            (elpaca--fail p (format "Unable to check out ref: %S " (string-trim stderr))))))
      (unless (eq (elpaca--status p) 'failed)
        (elpaca--update-info
         p
         (if target (format "%S ref checked out" target) "Default ref checked out")
         'ref-checked-out)
        (elpaca--continue-build p)))))

(defun elpaca--checkout-ref (e)
  "Checkout E's :ref. Handles :branch and :tag recipe keyword syntatic sugar."
  (elpaca--update-info e "Checking out repo ref")
  (let* ((default-directory (elpaca<-repo-dir e))
         (package           (elpaca<-package e))
         (recipe            (elpaca<-recipe e))
         (ref               (plist-get recipe :ref))
         (remotes           (plist-get recipe :remotes))
         (branch            (or (plist-get (cdr-safe (car-safe remotes)) :branch)
                                (plist-get recipe :branch)))
         (tag               (plist-get recipe :tag)))
    (unless remotes (signal 'wrong-type-argument `((stringp listp) ,remotes ,recipe)))
    (when (or ref branch tag)
      (cond
       ((and ref branch) (warn "Recipe :ref overriding :branch %S" recipe))
       ((and ref tag)    (warn "Recipe :ref overriding :tag %S" recipe))
       ((and tag branch) (error "Recipe :ref ambiguous :tag and :branch %S" recipe))))
    (let* ((process
            (make-process
             :name     (format "elpaca-fetch-%s" package)
             :command  '("git" "fetch" "--all")
             :filter   (lambda (process output)
                         (elpaca--process-filter process output "fatal" 'failed))
             :sentinel #'elpaca--checkout-ref-process-sentinel)))
      (process-put process :elpaca e)
      (setf (elpaca<-process e) process))))

(defun elpaca--check-status (e)
  "Called when one of an E's dependencies change status.
Kick off next build step, and/or change E's status."
  (unless (eq (elpaca--status e) 'finished)
    (let (failed blocked)
      (cl-loop for dependency in (elpaca<-dependencies e)
               for status = (elpaca--status dependency)
               unless (eq status 'finished)
               do (push (elpaca<-package dependency)
                        (if (eq status 'failed) failed blocked)))
      (cond
       (failed (elpaca--fail e (format "Failed dependencies: %S" failed)))
       (blocked (elpaca--update-info
                 e (format "Blocked by dependencies: %s" blocked) 'blocked))
       (t (elpaca--continue-build e))))))

(defun elpaca--clone-process-sentinel (process _event)
  "Sentinel for clone PROCESS."
  (let ((p   (process-get process :elpaca))
        (raw (process-get process :raw-output)))
    (if (and (string-match-p "fatal" raw) (not (string-match-p "already exists" raw)))
        (elpaca--fail p (nth 2 (car (elpaca<-log p))))
      (elpaca--continue-build p))))

(defun elpaca--clone (e)
  "Clone E's repo to `elpaca-directory'."
  (let* ((recipe  (elpaca<-recipe   e))
         (package (plist-get recipe :package))
         (depth   (plist-get recipe :depth))
         (repodir (elpaca<-repo-dir e))
         (URI     (elpaca--repo-uri recipe))
         (default-directory elpaca-directory))
    (push 'cloning (elpaca<-statuses e))
    (let ((process
           (make-process
            :name     (format "elpaca-clone-%s" package)
            :command  `("git" "clone"
                        ;;@TODO: Some refs will need a full clone or specific branch.
                        ,@(when depth
                            (list "--depth" (number-to-string depth) "--no-single-branch"))
                        ,URI ,repodir)
            :filter   (lambda (process output)
                        (elpaca--process-filter
                         process output
                         "\\(?:^\\(?:Password\\|Username\\|passphrase\\)\\)" 'blocked))
            :sentinel #'elpaca--clone-process-sentinel)))
      (process-put process :elpaca e)
      (setf (elpaca<-process e) process))))

(defun elpaca-generate-autoloads (package dir)
  "Generate autoloads in DIR for PACKAGE."
  (let* ((auto-name (format "%s-autoloads.el" package))
         (output    (expand-file-name auto-name dir))
         (autoload-timestamps nil)
         (backup-inhibited t)
         (version-control 'never)
         ;; Prevents spurious parens in autoloads
         (left-margin 0))
    (unless (file-exists-p output)
      (require 'autoload)
      (let ((generated-autoload-file output)
            (find-file-hook nil) ;; Don't clobber recentf
            (write-file-functions nil))
        (write-region (autoload-rubric output nil 'feature) nil output nil 'silent)
        (if (fboundp 'make-directory-autoloads)
            (make-directory-autoloads dir output)
          ;; Compatibility for Emacs < 28.1
          (with-no-warnings (update-directory-autoloads dir)))))
    (when-let ((buf (find-buffer-visiting output)))
      (kill-buffer buf))
    auto-name))

(defun elpaca--generate-autoloads-async-process-sentinel (process event)
  "PROCESS autoload generation EVENT."
  (when-let (((equal event "finished\n"))
             (e (process-get process :elpaca))
             ((not (eq (elpaca--status e) 'failed))))
    (elpaca--update-info e "Autoloads Generated")
    (elpaca--continue-build e)))

(defun elpaca--generate-autoloads-async (e)
  "Generate E's autoloads.
Async wrapper for `elpaca-generate-autoloads'."
  (let* ((emacs             (elpaca--emacs-path))
         (package           (elpaca<-package  e))
         (build-dir         (elpaca<-build-dir e))
         (default-directory build-dir)
         (elpaca            (expand-file-name "repos/elpaca/" elpaca-directory))
         (command
          (list emacs "-Q"
                "-L" elpaca
                "-L" build-dir ; Is this necessary?
                "-l" (expand-file-name "elpaca.el" elpaca)
                "--batch" "--eval"
                (format "(elpaca-generate-autoloads %S %S)" package build-dir)))
         (process
          (make-process
           :name     (format "elpaca-autoloads-%s" package)
           :command  command
           :filter   #'elpaca--process-filter
           :sentinel #'elpaca--generate-autoloads-async-process-sentinel)))
    (process-put process :elpaca e)
    (elpaca--update-info e (format "Generating autoloads: %s" default-directory) 'autoloads)))

(defun elpaca--activate-package (e)
  "Activate E's package."
  (elpaca--update-info e "Activating package" 'activation)
  (let* ((build-dir (elpaca<-build-dir e))
         (default-directory build-dir)
         (package           (elpaca<-package e))
         (autoloads         (expand-file-name (format "%s-autoloads.el" package))))
    (cl-pushnew default-directory load-path)
    ;;@TODO: condition on a slot we set on the e to indicate cached recipe?
    (elpaca--update-info e "Package build dir added to load-path")
    (if (and elpaca-cache-autoloads (file-exists-p autoloads))
        (let ((forms nil))
          (elpaca--update-info e "Caching autoloads")
          (with-temp-buffer
            (insert-file-contents autoloads)
            (goto-char (point-min))
            (condition-case _
                (while t (push (read (current-buffer)) forms))
              ((end-of-file)))
            (push `(let ((load-file-name ,autoloads)
                         (load-in-progress t)
                         (package ,package))
                     (condition-case err
                         (eval '(progn ,@(nreverse forms)) t)
                       ((error) (warn "Error loading %S autoloads: %S" package err))))
                  (elpaca-q<-autoloads (car (last elpaca--queues (1+ (elpaca<-queue-id e)))))))
          (elpaca--update-info e "Autoloads cached"))
      (condition-case err
          (progn
            (load autoloads nil 'nomessage)
            (elpaca--update-info e "Package activated" 'activated))
        ((error) (elpaca--update-info
                  e (format "Failed to load %S: %S" autoloads err) 'failed-to-activate))))
    (elpaca--continue-build e)))

(defun elpaca--byte-compile-process-sentinel (process event)
  "PROCESS byte-compilation EVENT."
  (when-let (((equal event "finished\n"))
             (e (process-get process :elpaca))
             ((not (eq (elpaca--status e) 'failed))))
    (elpaca--update-info e "Successfully byte compiled")
    (elpaca--continue-build e)))

(defun elpaca--byte-compile (e)
  "Byte compile E's package."
  ;; Assumes all dependencies are 'built
  (elpaca--update-info e "Byte compiling" 'byte-compilation)
  (let* ((build-dir         (elpaca<-build-dir e))
         (default-directory build-dir)
         (emacs             (elpaca--emacs-path))
         (dependency-dirs
          (cl-loop for item in (elpaca-dependencies (intern (elpaca<-package e))
                                                    elpaca-ignored-dependencies)
                   when item
                   for build-dir = (elpaca<-build-dir (elpaca-alist-get item (elpaca--queued)))
                   when build-dir collect build-dir))
         (program `(progn
                     (mapc (lambda (dir) (let ((default-directory dir))
                                           (add-to-list 'load-path dir)
                                           (normal-top-level-add-subdirs-to-load-path)))
                           ',(append dependency-dirs (list build-dir)))
                     (byte-recompile-directory ,build-dir 0 'force)))
         (print-level nil)
         (print-circle nil)
         (process
          (make-process
           :name     (format "elpaca-byte-compile-%s" (elpaca<-package e))
           :command  `(,emacs "-Q" "--batch" "--eval" ,(format "%S" program))
           :filter   #'elpaca--process-filter
           :sentinel #'elpaca--byte-compile-process-sentinel)))
    (process-put process :elpaca e)))

(defun elpaca-dependencies (item &optional ignore recurse)
  "Return recursive list of ITEM's dependencies.
IGNORE may be a list of symbols which are not included in the resulting list.
RECURSE is used to track recursive calls."
  (if-let ((e (or (elpaca-alist-get item (elpaca--queued))
                  (unless (member item elpaca-ignored-dependencies)
                    (elpaca<-create item))))
           (dependencies (elpaca--dependencies e)))
      (let ((transitives (cl-loop for dependency in dependencies
                                  for name = (car dependency)
                                  unless (memq name ignore) collect
                                  (cons name (elpaca-dependencies name ignore 'recurse)))))
        (delete-dups (flatten-tree transitives)))
    (when recurse item)))

(defun elpaca-dependents (item &optional recurse)
  "Return recursive list of packages which depend on ITEM.
RECURSE is used to keep track of recursive calls."
  (if-let ((e (elpaca-alist-get item (elpaca--queued)))
           (dependents (elpaca<-dependents e)))
      (let ((transitives
             (cl-loop for dependent in dependents collect
                      (let ((i (intern (elpaca<-package dependent))))
                        (cons i (elpaca-dependents i 'recurse))))))
        (delete-dups (nreverse (flatten-tree transitives))))
    (when recurse item)))

;;;; COMMANDS/MACROS
;;;###autoload
(defmacro elpaca (order &rest body)
  "Install ORDER, then execute BODY.
If ORDER is `nil`, defer BODY until orders have been processed."
  (when (equal 'quote (car-safe order)) (setq order (cadr order)))
  (declare (indent 1))
  `(progn
     ,@(when body (list `(push ',(cons (elpaca--first order) body)
                               (elpaca-q<-forms (car elpaca--queues)))))
     ,@(unless (null order) (list `(elpaca--queue
                                    ,(if (equal '\` (car-safe order))
                                         order
                                       (list 'quote order)))))))

;;;###autoload
(defmacro elpaca-use-package (order &rest body)
  "Execute BODY in `use-package' declartion after ORDER is finished.
If the :disabled keyword is present in body, the package is completely ignored.
This happens regardless of the value associated with :disabled.
The expansion is a string indicating the package has been disabled."
  (declare (indent 1))
  (if (memq :disabled body)
      (format "%S :disabled by elpaca-use-package" order)
    `(elpaca ,order (use-package ,(elpaca--first order) ,@body))))

(defvar elpaca-ui-entries-function)
(declare-function elpaca-log--latest "elpaca-log")
;;;###autoload
(defun elpaca-try-package (&rest orders)
  "Try ORDERS.
Install the repo/build files on disk.
Activate the corresponding package for the current session.
ORDER's package is not made available during subsequent sessions."
  (interactive (list
                (if (equal current-prefix-arg '(4))
                    (read (format "(%s)" (read-string "elpaca-try-package: ")))
                  (let ((recipe (elpaca-menu-item
                                 nil nil nil
                                 (lambda (candidate)
                                   (not (elpaca-alist-get (car candidate)
                                                          (elpaca--queued)))))))
                    (append (list (intern (plist-get recipe :package)))
                            recipe)))))
  (setq elpaca-cache-autoloads nil)
  (require 'elpaca-log)
  (elpaca-log--latest)
  (dolist (order orders)
    ;;@FIX: wasteful to pad out the order to make it QUEUED.
    (elpaca--process (cons (elpaca--first order) (elpaca--queue order)))))

(defun elpaca--process (queued)
  "Process QUEUED elpaca."
  (let ((e (cdr queued)))
    (unless (memq (elpaca--status e) '(failed blocked)) (elpaca--continue-build e))))

(defun elpaca--process-queue (q)
  "Process elpacas in Q."
  (when elpaca--show-status (require 'elpaca-log) (elpaca-log "#unique !finished"))
  (mapc #'elpaca--process (reverse (elpaca-q<-elpacas q))))

;;;###autoload
(defun elpaca-process-queues ()
  "Process the incomplete queues."
  (if-let ((incomplete (cl-find 'incomplete (reverse elpaca--queues) :key #'elpaca-q<-status)))
      (elpaca--process-queue incomplete)
    (message "No incomplete queues to process")))

(defun elpaca--on-disk-p (item)
  "Return t if ITEM has an associated P and a build or repo dir on disk."
  (when-let ((e (elpaca-alist-get item (elpaca--queued))))
    (or (file-exists-p (elpaca<-repo-dir e)) (file-exists-p (elpaca<-build-dir e)))))

;;@INCOMPLETE: We need to determine policy for deleting dependencies.
;; Maybe skip dependencies which weren't declared or dependencies of a declaration.
;;@FIX: this should be interactive.
;;;###autoload
(defun elpaca-delete-package (force with-deps &optional package asker)
  "Remove a PACKAGE from all caches and disk.
If WITH-DEPS is non-nil dependencies other than ASKER are deleted.
If FORCE is non-nil do not confirm before deleting."
  (when (or force (yes-or-no-p (format "Delete package %S?" package)))
    (if-let ((e (elpaca-alist-get package (elpaca--queued))))
        (let ((repo-dir      (elpaca<-repo-dir  e))
              (build-dir     (elpaca<-build-dir e))
              (dependents    (delq asker (elpaca-dependents package)))
              (dependencies  (when with-deps
                               (elpaca-dependencies package elpaca-ignored-dependencies))))
          (if (cl-some #'elpaca--on-disk-p dependents)
              (message "Cannot delete %S unless dependents %S are deleted first"
                       package dependents)
            (when (file-exists-p repo-dir)  (delete-directory repo-dir  'recursive))
            (when (file-exists-p build-dir) (delete-directory build-dir 'recursive))
            (dolist (queue elpaca--queues)
              (setf (elpaca-q<-elpacas queue)
                    (cl-remove package (elpaca-q<-elpacas queue) :key #'car)))
            (when (equal (buffer-name) elpaca-log-buffer) (elpaca-status))
            (message "Deleted package %S" package)
            (when with-deps
              (dolist (dependency dependencies)
                (elpaca-delete-package 'force with-deps dependency package)))))
      (if-let ((recipe (elpaca-recipe package)))
          (progn
            (when-let ((repo-dir (elpaca-repo-dir recipe))) (delete-directory repo-dir 'recursive))
            (when-let ((build-dir (elpaca-build-dir recipe))) (delete-directory build-dir 'recursive)))
        (user-error "%S is not queued" package)))))

;;;###autoload
(defun elpaca-rebuild-package (item &optional hide)
  "Rebuild ITEM's associated package.
If HIDE is non-nil, do not display `elpaca-log-buffer'."
  (interactive (list (intern (completing-read
                              "Rebuild package: "
                              (sort (mapcar #'car (elpaca--queued)) #'string<)
                              nil t))))
  (if-let ((queued (assoc item (elpaca--queued))))
      (let ((e (cdr queued)))
        (elpaca--update-info e "Rebuilding" 'rebuilding)
        (setq elpaca-cache-autoloads nil)
        (setf (elpaca<-build-steps e)
              (cl-remove-if (lambda (step) (member step '(elpaca--clone
                                                          elpaca--add-remotes
                                                          elpaca--checkout-ref
                                                          elpaca--clone-dependencies)))
                            (copy-tree elpaca-build-steps)))
        (setf (elpaca<-queue-time e) (current-time))
        (setf (elpaca<-statuses e) '(queued))
        (unless hide (require 'elpaca-log) (elpaca-log--latest))
        (elpaca--process queued))
    (user-error "Package %S is not queued" item)))

(defun elpaca--log-updates-process-sentinel (process event)
  "Handle PROCESS EVENT."
  (when-let (((equal event "finished\n"))
             (e (process-get process :elpaca)))
    (elpaca--update-info e "End Update log" 'updates-logged)
    (elpaca--continue-build e)))

;;@INCOMPLETE:
;; What do we actually want to log here?
;; If the user is on a different branch which has an upstream?
;; Or do we strictly log the difference between the recipe's declared ref and upstream?
;; Probably the latter, because that's the only case we can automatically update.
;; Anything else will require user intervention. ~ NV [2022-03-03]
(defun elpaca--log-updates (e)
  "Log E's fetched commits."
  (elpaca--update-info e "Logging updates" 'log-updates)
  (let* ((default-directory (elpaca<-repo-dir e))
         (recipe (elpaca<-recipe e))
         (remotes (plist-get recipe :remotes))
         (remote (elpaca--first remotes))
         (process (make-process
                   :name (format "elpaca-log-updates-%s" (elpaca<-package e))
                   ;; Pager will break this process. Complains about terminal functionality.
                   :command
                   (list "git" "--no-pager" "log" (elpaca--first remote) "..." "HEAD")
                   :filter   #'elpaca--process-filter
                   :sentinel #'elpaca--log-updates-process-sentinel)))
    (process-put process :elpaca e)))

(defun elpaca--fetch-process-sentinel (process event)
  "Handle PROCESS EVENT."
  (when-let (((equal event "finished\n"))
             (e (process-get process :elpaca)))
    (elpaca--update-info e "Updates fetched" 'updates-fetched)
    (elpaca--continue-build e)))

(defun elpaca--fetch (e)
  "Fetch E's remote's commits."
  (let* ((default-directory (elpaca<-repo-dir e))
         (process (make-process
                   :name (format "elpaca-fetch-%s" (elpaca<-package e))
                   :command  '("git" "fetch" "--all")
                   :filter   #'elpaca--process-filter
                   :sentinel #'elpaca--fetch-process-sentinel)))
    (process-put process :elpaca e)))

;;;###autoload
(defun elpaca-fetch (item &optional hide)
  "Fetch ITEM's associated package remote commits.
This does not merge changes or rebuild the packages.
If HIDE is non-nil don't display `elpaca-log-buffer'."
  (interactive
   (list (let ((item (completing-read "Fetch updates: "
                                      (sort (mapcar #'car (elpaca--queued)) #'string<)
                                      nil t)))
           (if (string-empty-p item)
               (user-error "No package selected")
             (intern item)))))
  (if-let ((queued (assoc item (elpaca--queued))))
      (let ((e (cdr queued)))
        (elpaca--update-info e "Fetching updates" 'fetching-updates)
        (setf (elpaca<-build-steps e) (list #'elpaca--fetch #'elpaca--log-updates))
        (setf (elpaca<-queue-time e) (current-time))
        (elpaca--process queued)
        (unless hide (elpaca-status)))
    (user-error "Package %S is not queued" item)))

;;;###autoload
(defun elpaca-fetch-all (&optional hide)
  "Fetch remote commits for queued elpacas.
If HIDE is non-nil, do not show `elpaca-log-buffer'."
  (interactive "P")
  (cl-loop for (item . _) in (cl-remove-duplicates (elpaca--queued) :key #'car)
           do (elpaca-fetch item hide)))

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

;; @TODO Implement these:
;; (defun elpaca-on-default-branch-p (item)
;;   (if-let ((order (elpaca-alist-get item (elpaca--queued))))
;;       (let* ((default-directory (elpaca-repo-dir order)))
;;         (
;;          ;;git ls-remote --symref REMOTE HEAD
;;          ;;git symbolic-ref HEAD  vs :branch
;;          ;; (defun elpaca-ui--local-branch-behind-p (package)

(defun elpaca-unshallow (item)
  "Convert ITEM's repo to an unshallow repository."
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
  "Load LOCKFILE.
If FORCE is non-nil,."
  (interactive "fLockfile: ")
  (message "%S" lockfile))

(defun elpaca-write-lockfile (path)
  "Write lockfile to PATH for current state of package repositories."
  (interactive "FWrite lockfile to: ")
  (elpaca--write-file path
    (pp (nreverse
         (cl-loop with seen
                  for (item . p) in (elpaca--queued)
                  unless (member item seen)
                  for rev =
                  (let ((default-directory (elpaca<-repo-dir p)))
                    (elpaca-with-process
                        (elpaca-process-call "git" "rev-parse" "HEAD")
                      (when success (string-trim stdout))))
                  when rev
                  collect (cons item (plist-put (copy-tree (elpaca<-recipe p)) :ref rev))
                  do (push item seen))))))

(provide 'elpaca)
;;; elpaca.el ends here
