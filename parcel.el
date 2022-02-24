;;; parcel.el --- An elisp package manager           -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nicholas Vollmer

;; Author: Nicholas Vollmer
;; URL: https://github.com/progfolio/parcel
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

;; An elisp package manager

;;; Code:
(eval-and-compile (require 'cl-lib))
(require 'text-property-search)
(require 'parcel-process)
(require 'tabulated-list)

(declare-function autoload-rubric "autoload")
(declare-function info-initialize "info")
(declare-function url-filename    "url-parse")
(declare-function url-host        "url-parse")
(defvar autoload-timestamps)
(defvar generated-autoload-file)
(defvar Info-directory-list)

(defgroup parcel nil
  "An elisp package manager."
  :group 'parcel
  :prefix "parcel-")

(defface parcel-finished
  '((t (:weight bold :foreground "#00FF00")))
  "Indicates an order is finished.")

(defface parcel-blocked
  '((t (:weight bold :foreground "#FFC1CC")))
  "Indicates an order is blocked.")

(defface parcel-failed
  '((t (:weight bold :foreground "#FF1818")))
  "Indicates an order has failed.")

(defvar parcel--init-complete nil
  "When non-nil prevent `parcel-after-init-hook' from being run.")

(defcustom parcel-after-init-hook nil
  "Parcel's analogue to `after-init-hook'.
This is run after all orders queued during init have finished processing.
It is only run once after init.
Note a blocked process will prevent this hook from being run."
  :type 'hook)

(defcustom parcel-cache-autoloads t
  "If non-nil, cache package autoloads and load all at once.
Results in faster start-up time.
However, loading errors will prevent later package autoloads from loading."
  :type 'boolean)

(defcustom parcel-cache-menu-items t
  "When non-nil, menu-items ares cached. Speeds up init load."
  :type 'boolean)

(defcustom parcel-cache-orders t
  "When non-nil, orders ares cached. Speeds up init load."
  :type 'boolean)

(defcustom parcel-directory (expand-file-name "parcel" user-emacs-directory)
  "Location of the parcel package store."
  :type 'directory)

(defvar parcel-cache-directory (expand-file-name "cache" parcel-directory)
  "Location of the cache directory.")

(defcustom parcel-display-status-buffer-during-init nil
  "When non-nil, display `parcel-status-buffer' during init.
This can slow down your init substantially.
Only recommended for troubleshooting."
  :type 'boolean)

(defcustom parcel-makeinfo-executable (executable-find "makeinfo")
  "Path of the makeinfo executable."
  :type 'string)

(defcustom parcel-install-info-executable
  (executable-find "install-info")
  "Path of the install-info executable."
  :type 'string)

(defcustom parcel-build-steps
  (list #'parcel--clone
        #'parcel--add-remotes
        #'parcel--checkout-ref
        #'parcel--run-pre-build-commands
        #'parcel--clone-dependencies
        #'parcel--link-build-files
        #'parcel--byte-compile
        #'parcel--generate-autoloads-async
        #'parcel--compile-info
        #'parcel--install-info
        #'parcel--add-info-path
        #'parcel--run-post-build-commands
        #'parcel--activate-package)
  "List of steps which are run when installing/building a package."
  :type 'list)

(defvar parcel--autoloads-cache nil "Cache for autoload forms.")

;;@TODO: Each package's configuration should be stored in an alist first
;; that way we can remove it if a package fails to install.
(defvar parcel--post-process-forms '(lambda ())
  "Forms to be executed after orders are processed.")

(defvar parcel--processed-order-count 0
  "Used to tell whent the queue has finished running.")

(defvar parcel-default-files-directive
  '("*.el" "*.el.in" "dir"
    "*.info" "*.texi" "*.texinfo"
    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo" "lisp/*.el"
    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"))
  "Default value for the `:files' directive in recipes.
It is also spliced in at any point where the `:defaults' keyword
is used in a `:files' directive.")

(defun parcel-order-defaults (_order)
  "Default order modifications. Matches any order."
  (list :protocol 'https :remotes "origin" :inherit t :depth 1))

(defcustom parcel-order-functions (list #'parcel-order-defaults)
  "Abnormal hook run to alter orders.
Each element must be a unary function which accepts an order.
An order may be nil, a symbol naming a package, or a plist.
The function may return nil or a plist to be merged with the order.
This hook is run via `run-hook-with-args-until-success'."
  :type 'hook)

(defun parcel-recipe-defaults (order)
  "Default ORDER modifications. Matches any ORDER."
  (unless (plist-get order :files)
    (list :files (list :defaults))))

(defcustom parcel-recipe-functions (list #'parcel-recipe-defaults)
  "Abnormal hook run to alter recipes.
Each element must be a unary function which accepts an recipe plist.
The function may return nil or a plist to be merged with the recipe.
This hook is run via `run-hook-with-args-until-success'."
  :type 'hook)

(defcustom parcel-menu-functions '(parcel-menu-org parcel-menu-melpa parcel-menu-gnu-elpa-mirror)
  "Abnormal hook to lookup packages in menus.
Each function is passed a request, which may be any of the follwoing symbols:
  - `index`
     Must return a alist of the menu's package candidates.
     Each candidate is a cell of form:
     (PACKAGE-NAME . (:source SOURCE-NAME :recipe RECIPE-PLIST))
  - `update`
     Updates the menu's package candidate list."
  :type 'hook)

(defvar parcel-ignored-dependencies
  (list 'emacs 'cl-lib 'cl-generic 'esxml 'nadvice 'org 'org-mode 'map 'seq 'json)
  "Built in packages.
Ignore these unless the user explicitly requests they be installed.")

(defvar parcel-overriding-prompt nil "Overriding prompt for interactive functions.")

(defun parcel--read-menu-cache ()
  "Read the menu-cache."
  (when-let ((cache (expand-file-name "menu-items.el" parcel-cache-directory))
             ((file-exists-p cache)))
    (with-temp-buffer
      (insert-file-contents cache)
      (condition-case err
          (read (buffer-string))
        ((error) (message "Parcel could not read menu-items.el: %S" err))))))

(defvar parcel-menu--candidates-cache
  (when parcel-cache-menu-items (parcel--read-menu-cache))
  "Cache for menu candidates.")

(defvar parcel--package-requires-regexp
  "\\(?:^;+[[:space:]]*Package-Requires[[:space:]]*:[[:space:]]*\\([^z-a]*?$\\)\\)"
  "Regexp matching the Package-Requires metadata in an elisp source file.")

(defvar parcel-recipe-keywords (list :branch :depth :fork :host :nonrecursive
                                     :package :post-build :pre-build :protocol
                                     :remote :repo)
  "Recognized parcel recipe keywords.")

(defvar parcel--queued-orders nil "List of queued orders.")

(defvar parcel--order-queue-start-time nil
  "Time used to keep order logs relative to start of queue.")

(defconst parcel-status-buffer "*Parcel*")

(defun parcel-merge-plists (&rest plists)
  "Return plist with set of unique keys from PLISTS.
Values for each key are that of the right-most plist containing that key."
  (let ((plists (delq nil plists))
        current plist)
    (while (setq current (pop plists))
      (while current (setq plist (plist-put plist (pop current) (pop current)))))
    plist))

(defmacro parcel--write-file (file &rest body)
  "Write FILE using BODY.
`standard-output' and print variables are lexically bound for convenience.
e.g. elisp forms may be printed via `prin1'."
  (declare (indent 1) (debug all))
  `(let ((coding-system-for-write 'utf-8))
     (with-temp-file ,file
       (let* ((standard-output (current-buffer))
              (print-circle nil)
              (print-level  nil)
              (print-length nil))
         ,@body
         nil))))

(defun parcel--write-menu-cache ()
  "Write menu item cache to disk."
  (unless (file-exists-p parcel-cache-directory)
    (make-directory parcel-cache-directory))
  (parcel--write-file (expand-file-name "menu-items.el" parcel-cache-directory)
    (prin1 parcel-menu--candidates-cache)))

(defun parcel-menu--candidates ()
  "Return alist of `parcel-menu-functions' candidates."
  (or parcel-menu--candidates-cache
      (prog1 (setq parcel-menu--candidates-cache
                   (sort (apply #'append
                                (cl-loop for fn in parcel-menu-functions
                                         for index = (funcall fn 'index)
                                         when index collect index))
                         (lambda (a b) (string-lessp (car a) (car b)))))
        (when parcel-cache-menu-items (parcel--write-menu-cache)))))

;;@TODO: clean up interface.
;;;###autoload
(defun parcel-menu-item (&optional interactive symbol menus filter)
  "Return menu item matching SYMBOL in MENUS or `parcel-menu-functions'.
If SYMBOL is nil, prompt for it.
If INTERACTIVE is equivalent to \\[universal-argument] prompt for MENUS.
FILTER must be a function which accepts a candidate.
If it returns nil, the candidate is not considered for selection."
  (interactive "P")
  (let* ((menus (if interactive
                    (mapcar #'intern-soft
                            (cl-remove-duplicates
                             (completing-read-multiple
                              "Menus: "
                              parcel-menu-functions
                              nil 'require-match)
                             :test #'equal))
                  (or menus parcel-menu-functions (user-error "No menus found"))))
         (parcel-menu-functions menus)
         (candidates (if filter
                         (cl-remove-if-not filter (parcel-menu--candidates))
                       (parcel-menu--candidates)))
         (symbol (or symbol
                     (intern-soft
                      (completing-read (or parcel-overriding-prompt "Package: ")
                                       candidates nil t))))
         (candidate (alist-get symbol candidates))
         (recipe (plist-get candidate :recipe)))
    (if (called-interactively-p 'interactive)
        (progn
          (unless recipe (user-error "No menu recipe for %s" symbol))
          (message "%S menu recipe for %s: %S"
                   (plist-get candidate :source) symbol recipe))
      recipe)))

(defsubst parcel--inheritance-disabled-p (plist)
  "Return t if PLIST explicitly has :inherit nil key val, nil otherwise."
  (when-let ((member (plist-member plist :inherit)))
    (not (cadr member))))

;;;###autoload
(defun parcel-recipe (&optional item)
  "Return recipe computed from ITEM.
ITEM is any of the following values:
  - nil. The order is prompted for.
  - a symbol which will be looked up via `parcel-menu-functions'
  - an order list."
  (interactive)
  (let ((parcel-overriding-prompt "Recipe: ")
        (interactive (called-interactively-p 'interactive))
        package
        ingredients)
    (cond
     ((or (null item) (symbolp item))
      (let ((menu-item (parcel-menu-item nil item)))
        (unless menu-item (user-error "No menu-item for %S" item))
        (push (run-hook-with-args-until-success 'parcel-order-functions item)
              ingredients)
        (push menu-item ingredients)))
     ((listp item)
      (setq package (pop item))
      (unless (parcel--inheritance-disabled-p item)
        (let ((mods (run-hook-with-args-until-success 'parcel-order-functions item)))
          (push mods ingredients)
          (when (or (plist-get item :inherit) (plist-get mods :inherit))
            (push (parcel-menu-item nil package) ingredients))))
      (setq ingredients (append ingredients (list item))))
     (t (signal 'wrong-type-argument `((null symbolp listp) . ,item))))
    (if-let ((recipe (apply #'parcel-merge-plists ingredients)))
        (progn
          (unless (plist-get recipe :package)
            (setq recipe (plist-put recipe :package (format "%S" package))))
          (setq recipe
                (parcel-merge-plists
                 recipe
                 (run-hook-with-args-until-success 'parcel-recipe-functions recipe)))
          (if interactive (message "%S" recipe)) recipe)
      (when interactive (user-error "No recipe for %S" package)))))

(defsubst parcel--emacs-path ()
  "Return path to running Emacs."
  (concat invocation-directory invocation-name))

(defsubst parcel--repo-name (string)
  "Return repo name portion of STRING."
  (substring string (1+ (string-match-p "/" string))))

(defsubst parcel--repo-user (string)
  "Return user name portion of STRING."
  (substring string 0 (string-match-p "/" string)))

(defun parcel--full-repo-protocol-p (string)
  "Return t if STRING specifies a protocol."
  ;;@TODO: this needs to be more robust.
  (and (string-match-p ":" string) t))

(defun parcel-repo-dir (recipe)
  "Return path to repo given RECIPE."
  (cl-destructuring-bind (&key url local-repo (repo url) fetcher (host fetcher) &allow-other-keys)
      recipe
    (expand-file-name
     (if (parcel--full-repo-protocol-p repo)
         (progn
           (require 'url-parse)
           (let ((url (url-generic-parse-url repo)))
             (string-join
              (list
               (or local-repo
                   (file-name-sans-extension
                    (replace-regexp-in-string
                     ".*/" ""
                     (url-filename
                      (url-generic-parse-url (or local-repo repo))))))
               "_"
               (url-host url))
              ".")))
       ;;repo-or-local-repo.user.host
       (string-join (list (or local-repo (parcel--repo-name repo))
                          (parcel--repo-user repo)
                          (symbol-name host))
                    "."))
     (expand-file-name "repos/" parcel-directory))))

(defun parcel-build-dir (recipe)
  "Return RECIPE's build dir."
  (expand-file-name (plist-get recipe :package)
                    (expand-file-name "builds/" parcel-directory)))

(defun parcel--repo-uri (recipe)
  "Return repo URI from RECIPE."
  (cl-destructuring-bind (&key (protocol 'https)
                               url
                               fetcher
                               (host fetcher)
                               (repo url) &allow-other-keys)
      recipe
    (if (parcel--full-repo-protocol-p repo)
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

(cl-defstruct (parcel-order (:constructor parcel-order-create)
                            (:type list)
                            (:named))
  "Order object for queued processing."
  package recipe build-steps statuses dependencies dependents body
  includes repo-dir build-dir files process log queue-time)

(defsubst parcel-order-status (order)
  "Return `car' of ORDER's statuses."
  (car (parcel-order-statuses order)))

(defun parcel--log-event (order text)
  "Store TEXT in ORDER's log.
Each event is of the form: (STATUS TIME TEXT)"
  (push (list (parcel-order-status order)
              (current-time)
              text)
        (parcel-order-log order)))

(defun parcel--events (&rest packages)
  "Return sorted event log string for PACKAGES.
If PACKAGES is nil, use all available orders."
  (let* ((packages (delete-dups (if packages (mapcar #'intern packages))))
         (queued (if packages
                     (cl-remove-if-not (lambda (cell) (member (car cell) packages))
                                       parcel--queued-orders)
                   parcel--queued-orders))
         (logs
          (mapcar (lambda (cell)
                    (let* ((package (car cell))
                           (order   (cdr cell))
                           (events (parcel-order-log order))
                           (log nil))
                      (dolist (event events log)
                        (let* ((status (nth 0 event))
                               (time   (nth 1 event))
                               (text   (nth 2 event))
                               (name   (propertize (symbol-name package)
                                                   'face
                                                   (pcase status
                                                     ('blocked  'parcel-blocked)
                                                     ('failed   'parcel-failed)
                                                     ('finished 'parcel-finished)
                                                     (_         '(:weight bold))))))
                          (push (cons time
                                      (format "[%s]%s %s"
                                              (format-time-string "%02s.%3N" (time-subtract time parcel--order-queue-start-time))
                                              (format "%-30s" (concat "(" name "):"))
                                              text))
                                log)))))
                  queued)))
    (mapconcat #'cdr (cl-sort (apply #'append logs) #'time-less-p :key #'car) "\n")))

(defun parcel--run-build-commands (commands)
  "Run build COMMANDS."
  (dolist (command (if (listp (car commands)) commands (list commands)))
    (if (cl-every #'stringp command)
        (parcel-with-process (apply #'parcel-process-call command)
          (if success
              (message stdout)
            (message "Build command error: %S" result)
            (error "Build command failed: %S" stderr)))
      (eval command t))))

(defsubst parcel-order-info (order)
  "Return ORDER's most recent log event info."
  (nth 2 (car (parcel-order-log order))))

(defun parcel-status-buffer-entries ()
  "Return list of queued orders entries suitable for `tabulated-list-entries'."
  (cl-loop for (item . order) in parcel--queued-orders
           for status = (parcel-order-status order)
           collect
           (list item (vector (propertize (parcel-order-package order)
                                          'face (cond
                                                 ((eq status 'blocked)  'parcel-blocked)
                                                 ((eq status 'finished) 'parcel-finished)
                                                 ((eq status 'failed)   'parcel-failed)
                                                 (t                     'default))
                                          'order order)
                              (symbol-name status)
                              (parcel-order-info order)))))

(defun parcel--print-order-status ()
  "Print ORDER's status in `parcel-status-buffer'."
  (let ((buffer (get-buffer parcel-status-buffer)))
    (unless buffer (parcel--initialize-process-buffer))
    (with-current-buffer (get-buffer-create parcel-status-buffer)
      (setq tabulated-list-entries (parcel-status-buffer-entries))
      (tabulated-list-init-header)
      (tabulated-list-print 'remember-pos 'update))))

(defun parcel--clean-order (order)
  "Return ORDER plist with cache data."
  (list
   :package      (parcel-order-package order)
   :recipe       (parcel-order-recipe order)
   :repo-dir     (parcel-order-repo-dir order)
   :build-dir    (parcel-order-build-dir order)
   :files        (parcel-order-files order)
   :dependencies (mapcar #'parcel--clean-order (parcel-order-dependencies order))))

(defun parcel--write-order-cache ()
  "Write order cache to disk."
  (unless (file-exists-p parcel-cache-directory)
    (make-directory parcel-cache-directory 'parents))
  (parcel--write-file (expand-file-name "cache/cache.el" parcel-directory)
    (prin1 (cl-loop for (_ . order) in (reverse parcel--queued-orders)
                    when (eq (parcel-order-status order) 'finished)
                    collect (parcel--clean-order order)))))

(defun parcel--cache-entry-to-order (plist)
  "Return decoded PLIST as order."
  (let ((order (apply #'parcel-order-create plist)))
    (setf (parcel-order-dependencies order)
          (mapcar #'parcel--cache-entry-to-order (parcel-order-dependencies order)))
    order))

(defun parcel--read-order-cache ()
  "Return cache alist or nil if not available."
  (when-let ((cache (expand-file-name "cache.el" parcel-cache-directory))
             ((file-exists-p cache)))
    (condition-case err
        (with-temp-buffer
          (insert-file-contents cache)
          (mapcar #'parcel--cache-entry-to-order (read (current-buffer))))
      ((error) (warn "Error reading parcel cache.el: %S" err)))))

(defvar parcel--order-cache (when parcel-cache-orders (parcel--read-order-cache))
  "Built package information cache.")

(defun parcel--continue-mono-repo-dependency (order)
  "Continue processing ORDER after it's mono-repo is in the proper state."
  (unless (member (parcel-order-statuses order) '(finished build-linked))
    (parcel--remove-build-steps
     order '(parcel--clone parcel--add-remotes parcel--checkout-ref))
    (parcel--run-next-build-step order)))

(defun parcel--update-order-info (order info &optional status)
  "Update ORDER STATUS.
Print the order status line in `parcel-status-buffer'.
If STATUS is non-nil and differs from ORDER's current STATUS,
signal ORDER's depedentents to check (and possibly change) their status.
If INFO is non-nil, ORDER's info is updated as well."
  (when status
    (push status (parcel-order-statuses order))
    (when (member status '(finished failed blocked))
      (mapc #'parcel--order-check-status (parcel-order-dependents order)))
    (when (eq status 'ref-checked-out)
      (mapc #'parcel--continue-mono-repo-dependency (parcel-order-includes order))))
  (when info (parcel--log-event order info))
  (when (or parcel-display-status-buffer-during-init parcel--init-complete)
    (parcel--print-order-status)))

(defun parcel--load-cached-autoloads ()
  "Load `parcel--autoloads-cache'."
  (eval `(progn ,@parcel--autoloads-cache) t))

(defun parcel--log-duration (order)
  "Return ORDER's log duration."
  ;;most recent event is car of log
  (let* ((log (parcel-order-log order))
         (end (nth 1 (car log))))
    (time-subtract end (parcel-order-queue-time order))))

(defun parcel--run-next-build-step (order)
  "Run ORDER's next build step with ARGS."
  (if-let ((next (pop (parcel-order-build-steps order))))
      (funcall next order)
    (let ((status (parcel-order-status order)))
      (if (eq  status 'finished)
          (cl-loop for order in (parcel-order-dependents order)
                   unless (eq (parcel-order-status order) 'finished)
                   do (parcel--order-check-status order))
        (unless (eq (parcel-order-status order) 'failed)
          (parcel--update-order-info
           order
           (concat  "✓ " (format-time-string "%s.%3N" (parcel--log-duration order)) " secs")
           'finished))
        (cl-incf parcel--processed-order-count)
        (when (= parcel--processed-order-count (length parcel--queued-orders))
          (when parcel--autoloads-cache (parcel--load-cached-autoloads))
          (funcall parcel--post-process-forms)
          (unless parcel--init-complete (run-hooks 'parcel-after-init-hook))
          (setq parcel--init-complete t)
          (setq parcel-cache-autoloads nil)
          (when parcel-cache-orders (parcel--write-order-cache)))))))

(defun parcel--queue-order (item &optional status)
  "Queue (ITEM . ORDER) in `parcel--queued-orders'.
If STATUS is non-nil, the order is given that initial status.
RETURNS order structure."
  (if (and (not after-init-time) (alist-get item parcel--queued-orders))
      (warn "%S already queued. Duplicate?" item)
    (let* ((status  (or status 'queued))
           (info    "Package queued")
           (package (if (listp item) (car item) item))
           (name (format "%S" package))
           (recipe  (condition-case err
                        (parcel-recipe item)
                      ((error)
                       (setq status 'failed
                             info (format "No recipe: %S" err))
                       nil)))
           (repo-dir (when recipe
                       (condition-case err
                           (parcel-repo-dir recipe)
                         ((error)
                          (setq status 'failed
                                info (format "Unable to determine repo dir: %S" err))))))
           (build-dir (when recipe (parcel-build-dir recipe)))
           (cached (cl-some (lambda (o) (when (equal (parcel-order-package o) name) o))
                            parcel--order-cache))
           (built-p nil)
           (order
            (parcel-order-create
             :package name :recipe recipe :statuses (list status)
             :build-steps
             (when recipe
               (if (file-exists-p build-dir)
                   (progn
                     (setq built-p t)
                     (list #'parcel--queue-dependencies
                           #'parcel--add-info-path
                           #'parcel--activate-package))
                 (when-let  ((steps (copy-tree parcel-build-steps)))
                   (when (file-exists-p repo-dir)
                     (setq steps (delq 'parcel--clone steps))
                     (when-let ((cached)
                                (cached-recipe (parcel-order-recipe cached))
                                ((eq recipe cached-recipe)))
                       (setq steps (cl-set-difference steps '(parcel--add-remotes
                                                              parcel--checkout-ref
                                                              parcel--dispatch-build-commands)))))
                   steps)))
             :repo-dir repo-dir :build-dir build-dir :queue-time (current-time)))
           (mono-repo
            (unless built-p
              (cl-some (lambda (cell)
                         (when-let ((queued (cdr cell))
                                    ((and repo-dir
                                          (equal repo-dir (parcel-order-repo-dir queued)))))
                           queued))
                       (reverse parcel--queued-orders)))))
      (prog1 order
        (push (cons package order) parcel--queued-orders)
        (if (not mono-repo)
            (parcel--update-order-info order info)
          (cl-pushnew order (parcel-order-includes mono-repo))
          (if (memq 'ref-checked-out (parcel-order-statuses mono-repo))
              (progn
                (parcel--remove-build-steps
                 order '(parcel--clone parcel--add-remotes parcel--checkout-ref))
                (parcel--run-next-build-step order))
            (parcel--update-order-info
             order (format "Waiting for monorepo %S" repo-dir) 'blocked)))))))

(defun parcel--add-remotes (order &optional recurse)
  "Add ORDER's repo remotes.
RECURSE is used to keep track of recursive calls."
  (let ((default-directory (parcel-order-repo-dir order))
        (recipe            (parcel-order-recipe   order)))
    (cl-destructuring-bind
        ( &key remotes
          ((:host recipe-host))
          ((:protocol recipe-protocol))
          ((:repo recipe-repo))
          &allow-other-keys)
        recipe
      (unless recurse (parcel--update-order-info order "Adding Remotes"))
      (pcase remotes
        ("origin" nil)
        ((and (pred stringp) remote)
         (parcel-process-call "git" "remote" "rename" "origin" remote))
        ((pred listp)
         (dolist (spec remotes)
           (if (stringp spec)
               (parcel--add-remotes
                (let ((copy (copy-parcel-order order)))
                  (setf (parcel-order-recipe copy)
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
                   (parcel-process-call
                    "git" "remote" "add" remote (parcel--repo-uri recipe))
                   (unless (equal remote "origin")
                     (parcel-process-call "git" "remote" "rename" "origin" remote))))))))
        (_ (parcel--update-order-info
            order
            (format "(wrong-type-argument ((stringp listp)) %S" remotes)
            'failed)))))
  (unless recurse (parcel--run-next-build-step order)))

(defun parcel--remove-build-steps (order steplist)
  "Remove each step in STEPLIST from ORDER."
  (setf (parcel-order-build-steps order)
        (cl-set-difference (parcel-order-build-steps order) steplist)))

(defun parcel--files (order &optional files nocons)
  "Return alist of ORDER :files to be symlinked: (PATH . TARGET PATH).
FILES and NOCONS are used recursively."
  (let* ((default-directory (parcel-order-repo-dir order))
         (build-dir         (parcel-order-build-dir order))
         (recipe            (parcel-order-recipe order))
         (files             (or files (plist-get recipe :files)))
         (exclusions        nil)
         (targets           nil)
         (with-subdirs      nil))
    (dolist (el files)
      (pcase el
        ((pred stringp) (push (or (file-expand-wildcards el) el) targets))
        (`(:exclude  . ,excluded)
         (push (parcel--files order excluded 'nocons) exclusions)
         nil)
        (:defaults
         (push (parcel--files order parcel-default-files-directive 'nocons) targets))
        (`(,subdir . ,paths)
         (cl-loop for path in paths
                  do (push (cons (expand-file-name path)
                                 (let ((default-directory build-dir))
                                   (expand-file-name path subdir)))
                           with-subdirs)))))
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

(defun parcel--link-build-files (order)
  "Link ORDER's :files into it's builds subdirectory."
  (parcel--update-order-info order "Linking build files")
  (let* ((build-dir (parcel-order-build-dir order))
         (files (or (parcel-order-files order)
                    (setf (parcel-order-files order) (parcel--files order)))))
    (when (file-exists-p build-dir) (delete-directory build-dir 'recusrive))
    (make-directory build-dir 'parents)
    (dolist (spec files)
      (let ((file   (car spec))
            (link   (cdr spec)))
        (make-directory (file-name-directory link) 'parents)
        (make-symbolic-link file link 'overwrite))))
  (parcel--update-order-info order "Build files linked" 'build-linked)
  (parcel--run-next-build-step order))

(defun parcel--add-info-path (order)
  "Add the ORDER's info to `Info-directory-list'."
  (parcel--update-order-info order "Adding Info path" 'info)
  (with-eval-after-load 'info
    (info-initialize)
    (cl-pushnew (parcel-order-build-dir order) Info-directory-list))
  (parcel--run-next-build-step order))

(defun parcel--process-filter (process output &optional pattern status)
  "Filter PROCESS OUTPUT.
PATTERN is a string which is checked against the entire process output.
If it matches, the order associated with process has its STATUS updated."
  (process-put process :raw-output (concat (process-get process :raw-output) output))
  (let* ((order  (process-get process :order))
         (result (process-get process :result))
         (lines  (split-string (concat result output) parcel-process-newline-regexp))
         (last-is-full-line-p (string-empty-p (car (last lines)))))
    (unless last-is-full-line-p
      (process-put process :result (car (last lines)))
      (setq lines (butlast lines)))
    (dolist (line lines)
      (unless (string-empty-p line)
        (parcel--update-order-info
         order line
         (when (and pattern (string-match-p pattern line))
           status))))
    (when (and pattern (string-match-p pattern output))
      (process-put process :result nil)
      (parcel--update-order-info order output status))))

(defun parcel--compile-info-process-sentinel (process event)
  "Sentinel for info compilation PROCESS EVENT."
  (let ((order  (process-get process :order)))
    (parcel--update-order-info
     order (if (equal event "finished\n")
               "Info compiled"
             (format "Failed to compile Info: %S" (string-trim event))))
    (parcel--run-next-build-step order)))

(defun parcel--compile-info (order)
  "Compile ORDER's .texi files."
  (parcel--update-order-info order "Compiling Info files")
  (if-let
      ((files
        (delq
         nil
         (mapcar
          (lambda (spec)
            (let ((repo-file  (car spec))
                  (build-file (cdr spec)))
              (when-let (((string-match-p "\\.texi\\(nfo\\)?$" repo-file))
                         (info (concat (file-name-sans-extension build-file) ".info"))
                         ((not (file-exists-p info))))
                (list repo-file "-o" info))))
          (or (parcel-order-files order)
              (setf (parcel-order-files order) (parcel--files order))))))
       (command `(,parcel-makeinfo-executable ,@(apply #'append files)))
       (process (make-process
                 :name (format "parcel-compile-info-%s" (parcel-order-package order))
                 :command command
                 :filter   #'parcel--process-filter
                 :sentinel #'parcel--compile-info-process-sentinel)))
      (process-put process :order order)
    (parcel--update-order-info order "No .info files found")
    (parcel--remove-build-steps order '(parcel--install-info parcel--add-info-path))
    (parcel--run-next-build-step order)))

(defun parcel--install-info (order)
  "Install ORDER's info files."
  (parcel--update-order-info order "Installing Info files")
  (when-let ((dir (expand-file-name "dir" (parcel-order-build-dir order)))
             ((not (file-exists-p dir)))
             ;;@OPTIMIZE: cl-loop
             (files
              (delq nil
                    (mapcar
                     (lambda (spec)
                       (let ((repo-file  (car spec))
                             (build-file (cdr spec)))
                         (cond
                          ((string-match-p "\\.info$" build-file)
                           build-file)
                          ((string-match-p "\\.texi\\(nfo\\)?$" repo-file)
                           (concat (file-name-sans-extension build-file) ".info")))))
                     (or (parcel-order-files order)
                         (setf (parcel-order-files order) (parcel--files order)))))))
    (dolist (file (cl-remove-if-not #'file-exists-p files))
      (parcel-with-process (parcel-process-call parcel-install-info-executable
                                                file dir)
        (unless success (parcel--update-order-info order result)))))
  (parcel--run-next-build-step order))

(defun parcel--dispatch-build-commands-process-sentinel (process event)
  "PROCESS EVENT."
  (let ((order (process-get process :order))
        (type  (process-get process :build-type)))
    (cond
     ((equal event "finished\n")
      (parcel--update-order-info order
                                 (format "%s steps finished" type)
                                 (intern (substring (symbol-name type) 1)))
      (parcel--run-next-build-step order))
     ((string-match-p "abnormally" event)
      (parcel--update-order-info
       order
       ;; We want the event prior to the last "exited abnormally" event.
       (nth 2 (car (last (parcel-order-log order) 2))))
      'failed))))

(defun parcel--dispatch-build-commands (order type)
  "Run ORDER's TYPE commands for.
TYPE is either the keyword :pre-build, or :post-build.
Each command is either an elisp form to be evaluated or a list of
strings to be executed in a shell context of the form:

  (\"executable\" \"arg\"...)

Commands are exectued in the ORDER's repository directory.
The keyword's value is expected to be one of the following:

  - A single command
  - A list of commands
  - nil, in which case no commands are executed.
    Note if :build is nil, :pre/post-build commands are not executed."
  (if-let ((recipe   (parcel-order-recipe order))
           (commands (plist-get recipe type)))
      (progn
        (parcel--update-order-info order (format "Running %S commands" type))
        (let* ((default-directory (parcel-order-repo-dir order))
               (emacs             (parcel--emacs-path))
               (program           `(progn
                                     (require 'parcel)
                                     (normal-top-level-add-subdirs-to-load-path)
                                     (parcel--run-build-commands ',commands)))
               (process (make-process
                         :name (format "parcel-%s-%s" type (plist-get recipe :package))
                         :command (list
                                   emacs "-Q"
                                   "-L" "./"
                                   "-L" (expand-file-name "parcel" parcel-directory)
                                   "--batch"
                                   "--eval" (let (print-level print-length)
                                              (format "%S" program)))
                         :filter   #'parcel--process-filter
                         :sentinel #'parcel--dispatch-build-commands-process-sentinel)))
          (process-put process :order order)
          (process-put process :build-type type)))
    (parcel--run-next-build-step order)))

(defun parcel--run-pre-build-commands (order)
  "Run ORDER's :pre-build commands."
  (parcel--dispatch-build-commands order :pre-build))

(defun parcel--run-post-build-commands (order)
  "Run ORDER's :post-build commands."
  (parcel--dispatch-build-commands order :post-build))

(defun parcel--dependencies (recipe)
  "Using RECIPE, compute package's dependencies.
If package's repo is not on disk, error."
  (let* ((default-directory (parcel-repo-dir recipe))
         (package (plist-get recipe :package))
         (pkg (expand-file-name (format "%s-pkg.el" package)))
         (defined (file-exists-p pkg))
         (name (format "%s.el" package))
         (main
          (or
           ;;@TODO: Should we have a recipe keyword to explicitly declare this?
           ;; e.g. :main, or something special in :files?
           (car (directory-files-recursively default-directory (format "^%s$" name)))
           ;; Best guess if there is no file matching the package name...
           (car (directory-files default-directory nil "\\.el$" 'nosort)))))
    (unless (file-exists-p default-directory)
      (error "Package repository not on disk: %S" recipe))
    (with-temp-buffer
      (insert-file-contents-literally (if defined pkg main))
      (goto-char (point-min))
      (if defined
          (eval (nth 4 (read (current-buffer))))
        (let ((case-fold-search t))
          (when (re-search-forward parcel--package-requires-regexp nil 'noerror)
            (condition-case err
                (read (match-string 1))
              (error "Unable to parse %S Package-Requires metadata: %S" main err))))))))

(defun parcel--queue-dependencies (order)
  "Queue ORDER's dependencies."
  (parcel--update-order-info order "Queueing Dependencies" 'queueing-deps)
  (let* ((recipe       (parcel-order-recipe order))
         (dependencies (parcel--dependencies recipe))
         (externals    (cl-loop for (dep . version) in dependencies
                                when (not (member dep parcel-ignored-dependencies))
                                collect (list dep version)))
         queued-deps)
    (if externals
        (progn
          ;; We do this in two steps so that ORDER is aware of all its
          ;; dependencies before any single dependency starts its build.
          ;; Otherwise a dependency may finish prior to other dependencies being
          ;; registered. This will cause the dependent order to become unblocked
          ;; multiple times and run its build steps simultaneously/out of order.
          (dolist (spec externals)
            (let* ((dependency (car spec))
                   (queued     (alist-get dependency parcel--queued-orders))
                   (dep-order  (or queued (parcel--queue-order dependency))))
              (setf (parcel-order-dependencies order)
                    (delete-dups (append (parcel-order-dependencies order) (list dep-order))))
              (cl-pushnew order (parcel-order-dependents dep-order))
              (push dep-order queued-deps)))
          (mapc #'parcel--run-next-build-step (nreverse queued-deps)))
      (parcel--update-order-info order "No external dependencies detected")
      (parcel--run-next-build-step order))))

;;@TODO: fix possible race similar to queue--dependencies.
(defun parcel--clone-dependencies (order)
  "Clone ORDER's dependencies."
  (parcel--update-order-info order "Cloning Dependencies" 'cloning-deps)
  (let* ((recipe       (parcel-order-recipe order))
         (dependencies (parcel--dependencies recipe))
         (emacs        (assoc 'emacs dependencies))
         ;;@TOOD: optimize with cl-loop
         (externals    (cl-remove-duplicates
                        (cl-remove-if (lambda (dependency)
                                        (member dependency parcel-ignored-dependencies))
                                      dependencies :key #'car))))
    (if (and emacs (version< emacs-version (cadr emacs)))
        (parcel--update-order-info
         order (format "Requires %S; running %S" emacs emacs-version) 'failed)
      (if externals
          ;;@TODO: Major Version conflict checks?
          (let ((finished 0))
            (dolist (spec externals)
              (let* ((dependency (car spec))
                     (queued     (alist-get dependency parcel--queued-orders))
                     (dep-order  (or queued (parcel--queue-order dependency)))
                     (included   (member dep-order (parcel-order-includes order)))
                     (blocked    (eq (parcel-order-status dep-order) 'blocked)))
                (setf (parcel-order-dependencies order)
                      (delete-dups (append (parcel-order-dependencies order) (list dep-order))))
                (cl-pushnew order (parcel-order-dependents dep-order))
                (if queued
                    (when (eq (parcel-order-status queued) 'finished) (cl-incf finished))
                  (if included
                      ;; Unblock dependency published in same repo...
                      (when blocked (parcel--clone-dependencies dep-order))
                    (unless blocked (parcel--run-next-build-step dep-order))))))
            (when (= (length externals) finished) ; Our dependencies beat us to the punch
              (parcel--run-next-build-step order)))
        (parcel--update-order-info order "No external dependencies detected")
        (parcel--run-next-build-step order)))))

(defun parcel--checkout-ref-process-sentinel (process event)
  "PROCESS EVENT."
  (when (equal event "finished\n")
    (let* ((order             (process-get process   :order))
           (recipe            (parcel-order-recipe   order))
           (default-directory (parcel-order-repo-dir order)))
      (cl-destructuring-bind ( &key remotes ref tag branch &allow-other-keys
                               &aux (remote (if (stringp remotes) remotes (caar remotes))))
          recipe
        (when (or ref tag branch)
          (parcel-with-process
              (apply #'parcel-process-call
                     `("git"
                       ,@(delq nil
                               (cond
                                (ref    (list "checkout" ref))
                                (tag    (list "checkout" (concat ".git/refs/tags/" tag)))
                                (branch (list "switch" "-C" branch
                                              (format "%s/%s" remote branch)))))))
            (unless success
              (parcel--update-order-info
               order (format "Unable to check out ref: %S " (string-trim stderr))
               'failed))))
        (unless (eq (parcel-order-status order) 'failed)
          (parcel--update-order-info order "Ref checked out" 'ref-checked-out)
          (parcel--run-next-build-step order))))))

(defun parcel--checkout-ref (order)
  "Checkout ORDER's :ref.
The :branch and :tag keywords are syntatic sugar and are handled here, too."
  (parcel--update-order-info order "Checking out repo ref")
  (let* ((default-directory (parcel-order-repo-dir order))
         (package           (parcel-order-package order))
         (recipe            (parcel-order-recipe order))
         (ref               (plist-get recipe :ref))
         (branch            (plist-get recipe :branch))
         (tag               (plist-get recipe :tag))
         (remotes           (plist-get recipe :remotes)))
    (unless remotes (signal 'wrong-type-argument `((stringp listp) ,remotes ,recipe)))
    (when (or ref branch tag)
      (cond
       ((and ref branch) (warn "Recipe :ref overriding :branch %S" recipe))
       ((and ref tag)    (warn "Recipe :ref overriding :tag %S" recipe))
       ((and tag branch) (error "Recipe :ref ambiguous :tag and :branch %S" recipe))))
    (let* ((process
            (make-process
             :name     (format "parcel-fetch-%s" package)
             :command  (list "git" "fetch" "--all")
             :filter   (lambda (process output)
                         (parcel--process-filter process output "fatal" 'failed))
             :sentinel #'parcel--checkout-ref-process-sentinel)))
      (process-put process :order order)
      (setf (parcel-order-process order) process))))

(defun parcel--set-header-line ()
  "Set header line format for `parcel-buffer'."
  (let ((counts nil)
        (queue-len (length parcel--queued-orders)))
    (dolist (queued parcel--queued-orders)
      (let ((status (parcel-order-status (cdr queued))))
        (if (alist-get status counts)
            (cl-incf (alist-get status counts))
          (push (cons status 1) counts))))
    (with-current-buffer (get-buffer-create parcel-status-buffer)
      (setq header-line-format
            (concat
             (propertize " Parcel " 'face '(:weight bold))
             " "
             (format "Queued: %d | %s(%.2f%%%%): %d | %s: %d | %s: %d"
                     queue-len
                     (propertize "Finished" 'face 'parcel-finished)
                     (if-let ((finished (alist-get 'finished counts)))
                         (* (/ (float finished) queue-len) 100)
                       0.00)
                     (or (alist-get 'finished counts) 0)
                     (propertize "Blocked" 'face 'parcel-blocked)
                     (or (alist-get 'blocked  counts) 0)
                     (propertize "Failed" 'face 'parcel-failed)
                     (or (alist-get 'failed   counts) 0)))))))

(defun parcel-status-buffer-line (order)
  "Return status string for ORDER."
  (let* ((package (parcel-order-package order))
         (status  (parcel-order-status  order))
         (name (format "%-30s"
                       (propertize package 'face
                                   (pcase status
                                     ('blocked  'parcel-blocked)
                                     ('failed   'parcel-failed)
                                     ('finished 'parcel-finished)
                                     (_         '(:weight bold)))))))
    (concat (propertize
             (format "%s (%s) %s" name (or status "?") (or (parcel-order-info order) ""))
             'read-only         t
             'front-sticky      t
             'package           package
             'order             order)
            " ")))

(defun parcel--order-check-status (order)
  "Called when one of an ORDER's dependencies have changed status.
Possibly kicks off next build step, or changes order status."
  (unless (eq (parcel-order-status order) 'finished)
    (let* ((statuses
            (mapcar (lambda (dependency)
                      (cons (parcel-order-package dependency)
                            (parcel-order-status  dependency)))
                    (parcel-order-dependencies order)))
           (blocked (cl-remove-if (lambda (status) (eq status 'finished))
                                  statuses :key #'cdr))
           (failed  (cl-remove-if-not (lambda (status) (eq status 'failed))
                                      statuses :key #'cdr)))
      (cond
       (failed
        (parcel--update-order-info
         order (format "Failed dependencies: %S" (mapcar #'car failed)) 'failed))
       (blocked
        (parcel--update-order-info
         order (format "Blocked by dependencies: %S" (mapcar #'car blocked)) 'blocked))
       ((cl-every (lambda (status) (eq (cdr status) 'finished)) statuses)
        (parcel--run-next-build-step order))))))

(defun parcel--clone-process-sentinel (process _event)
  "Sentinel for clone PROCESS."
  (let ((order      (process-get process :order))
        (raw-output (process-get process :raw-output)))
    (if (and (string-match-p "fatal" raw-output)
             (not (string-match-p "already exists" raw-output)))
        (parcel--update-order-info order
                                   (nth 2 (car (parcel-order-log order)))
                                   'failed)
      (parcel--run-next-build-step order))))

(defun parcel--clone (order)
  "Clone repo to `parcel-directory' from ORDER.
If FORCE is non-nil, ignore order queue."
  (let* ((recipe  (parcel-order-recipe order))
         (package (plist-get recipe :package))
         (depth   (plist-get recipe :depth))
         (repodir (parcel-order-repo-dir order))
         (URI     (parcel--repo-uri recipe))
         (default-directory parcel-directory))
    (push 'cloning (parcel-order-statuses order))
    (let ((process
           (make-process
            :name     (format "parcel-clone-%s" package)
            :command  `("git" "clone"
                        ;;@TODO: certain refs will necessitate full clone
                        ;; or specific branch...
                        ,@(when depth (list "--depth" (number-to-string depth)))
                        ,URI ,repodir)
            :filter   (lambda (process output)
                        (parcel--process-filter process output "\\(?:^\\(?:Password\\|Username\\)\\)" 'blocked))
            :sentinel #'parcel--clone-process-sentinel)))
      (process-put process :order order)
      (setf (parcel-order-process order) process))))

(defun parcel-generate-autoloads (package dir)
  "Generate autoloads in DIR for PACKAGE."
  (let* ((auto-name (format "%s-autoloads.el" package))
         (output    (expand-file-name auto-name dir))
         (autoload-timestamps nil)
         (backup-inhibited t)
         (version-control 'never))
    (unless (file-exists-p output)
      (require 'autoload)
      (let ((generated-autoload-file output)
            (find-file-hook nil) ;; Don't clobber recentf
            (write-file-functions nil))
        (write-region (autoload-rubric output nil 'feature) nil output nil 'silent)
        ;;@TODO: support older Emacs?
        (make-directory-autoloads dir output)))
    (when-let ((buf (find-buffer-visiting output)))
      (kill-buffer buf))
    auto-name))

(defun parcel--generate-autoloads-async-process-sentinel (process event)
  "PROCESS autoload generation EVENT."
  (when (equal event "finished\n")
    (let ((order  (process-get process :order)))
      (unless (eq (parcel-order-status order) 'failed)
        (parcel--update-order-info order "Autoloads Generated" 'autoloads-generated)
        (parcel--run-next-build-step order)))))

(defun parcel--generate-autoloads-async (order)
  "Generate ORDER's autoloads.
Async wrapper for `parcel-generate-autoloads'."
  (parcel--log-event order "Generating autoloads")
  (let* ((emacs             (parcel--emacs-path))
         (package           (parcel-order-package  order))
         (build-dir         (parcel-order-build-dir order))
         (default-directory build-dir)
         (parcel            (expand-file-name "parcel/" parcel-directory))
         (command
          (list emacs "-Q"
                "-L" parcel
                "-L" build-dir ; Is this necessary?
                "-l" (expand-file-name "parcel.el" parcel)
                "--batch" "--eval"
                (format "(parcel-generate-autoloads %S %S)" package build-dir)))
         (process
          (make-process
           :name     (format "parcel-autoloads-%s" package)
           :command  command
           :filter   #'parcel--process-filter
           :sentinel #'parcel--generate-autoloads-async-process-sentinel)))
    (process-put process :order order)))

(defun parcel--activate-package (order)
  "Activate ORDER's package."
  (parcel--update-order-info order "Activating package")
  (let* ((build-dir (parcel-order-build-dir order))
         (default-directory build-dir)
         (package           (parcel-order-package order))
         (autoloads         (expand-file-name (format "%s-autoloads.el" package))))
    (cl-pushnew default-directory load-path)
    ;;@TODO: condition on a slot we set on the order to indicate cached recipe?
    (parcel--update-order-info order "Package build dir added to load-path")
    (if (and parcel-cache-autoloads (file-exists-p autoloads))
        (let ((forms nil))
          (parcel--update-order-info order "Caching autoloads")
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
                  parcel--autoloads-cache))
          (parcel--update-order-info order "Autoloads cached"))
      (condition-case err
          (progn
            (load autoloads nil 'nomessage)
            (parcel--update-order-info order "Package activated" 'activated))
        ((error) (parcel--update-order-info
                  order (format "Failed to load %S: %S" autoloads err) 'failed-to-activate))))
    (parcel--run-next-build-step order)))

(defun parcel--byte-compile-process-sentinel (process event)
  "PROCESS byte-compilation EVENT."
  (when (equal event "finished\n")
    (let ((order (process-get process :order)))
      (unless (eq (parcel-order-status order) 'failed)
        (parcel--update-order-info order "Successfully byte compiled" 'byte-compiled)
        (parcel--run-next-build-step order)))))

(defun parcel--byte-compile (order)
  "Byte compile package from ORDER."
  ;; Assumes all dependencies are 'built
  (parcel--update-order-info order "Byte compiling")
  (let* ((build-dir         (parcel-order-build-dir order))
         (default-directory build-dir)
         (emacs             (parcel--emacs-path))
         (dependency-dirs (mapcar #'parcel-order-build-dir
                                  (parcel-order-dependencies order)))
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
           :name     (format "parcel-byte-compile-%s" (parcel-order-package order))
           :command  `(,emacs "-Q" "--batch" "--eval" ,(format "%S" program))
           :filter   #'parcel--process-filter
           :sentinel #'parcel--byte-compile-process-sentinel)))
    (process-put process :order order)))

(defun parcel-dependencies (item &optional recurse)
  "Return recursive list of ITEM's dependencies.
RECURSE is used to track recursive calls."
  (if-let ((order (alist-get item parcel--queued-orders))
           (dependencies (parcel--dependencies (parcel-order-recipe order))))
      (let ((transitives (cl-loop for dependency in dependencies
                                  for name = (car dependency)
                                  unless (eq name 'emacs)
                                  for ds = (parcel-dependencies name 'recurse)
                                  collect (append (list name) ds))))
        (delete-dups (delq 'emacs (flatten-tree transitives))))
    (when recurse item)))

(defun parcel-dependents (item &optional recurse)
  "Return recursive list of packages which depend on ITEM.
RECURSE is used to keep track of recursive calls."
  (if-let ((order (alist-get item parcel--queued-orders))
           (dependents (parcel-order-dependents order)))
      (let ((transitives
             (cl-loop
              for dependent in dependents
              collect
              (append (list (intern (parcel-order-package dependent))
                            (mapcar (lambda (o) (intern (parcel-order-package o)))
                                    (parcel-dependents dependent 'recurse)))))))
        (nreverse (flatten-tree transitives)))
    (when recurse order)))

;;;; COMMANDS/MACROS
(defun parcel-print-log (&rest packages)
  "Print log for PACKAGES."
  (interactive (completing-read-multiple "Log for Packages: "
                                         (mapcar #'car parcel--queued-orders)))
  (let ((queued (if packages
                    (cl-remove-if-not (lambda (package)
                                        (alist-get (intern package) parcel--queued-orders))
                                      packages)
                  (mapcar (lambda (queued) (symbol-name (car queued)))
                          parcel--queued-orders))))
    (unless queued (user-error "No queued packages by name: %S"  packages))
    (with-current-buffer (get-buffer-create "*Parcel Log*")
      (with-silent-modifications
        (erase-buffer)
        (insert (apply #'parcel--events queued))
        (pop-to-buffer-same-window (current-buffer)))
      (goto-char (point-min))
      (special-mode))))

(defvar parcel-status-buffer-list-format [("Package" 30 t) ("Status" 15 t) ("Info" 100 t)]
  "Format of `parcel-status-buffer' columns.")

(defun parcel--initialize-process-buffer (&optional display)
  "Initialize the parcel process buffer.
When DISPLAY is non-nil, display the buffer."
  (with-current-buffer (get-buffer-create parcel-status-buffer)
    (unless (derived-mode-p 'parcel-status-mode) (parcel-status-mode))
    (with-silent-modifications (erase-buffer))
    (setq tabulated-list-use-header-line nil
          tabulated-list-format parcel-status-buffer-list-format
          tabulated-list-entries (parcel-status-buffer-entries))
    (tabulated-list-init-header)
    (tabulated-list-print-fake-header)
    (tabulated-list-print 'remember-pos 'update)
    (parcel--set-header-line)
    (when display (pop-to-buffer-same-window (current-buffer)))))

;;;###autoload
(defun parcel-display-status-buffer ()
  "Diplay `parcel-status-buffer'."
  (interactive)
  (parcel--initialize-process-buffer 'display))

;;;###autoload
(defmacro parcel (order &rest body)
  "Install ORDER, then execute BODY.
If ORDER is `nil`, defer BODY until orders have been processed."
  (declare (indent 1))
  `(progn
     ,@(unless (null order) (list `(parcel--queue-order ,order)))
     (setq parcel--post-process-forms (append parcel--post-process-forms ',body))))

;;;###autoload
(defmacro parcel-use-package (order &rest body)
  "Execute BODY in `use-package' declartion after ORDER is finished.
If the :disabled keyword is present in body, the package is completely ignored.
This happens regardless of the value associated with :disabled.
The expansion is a string indicating the package has been disabled."
  (declare (indent 1))
  (if (member :disabled body)
      (format "%S :disabled by parcel-use-package" order)
    `(parcel ',order
       (use-package ,(if (listp order) (car order) order)
         ,@body))))

;;;###autoload
(defun parcel-try-package (order)
  "Try ORDER.
Install the repo/build files on disk.
Activate the corresponding package for the current session.
ORDER's package is not made available during subsequent sessions."
  (interactive (list
                (let ((recipe (parcel-menu-item
                               nil nil nil
                               (lambda (candidate)
                                 (not (alist-get (car candidate)
                                                 parcel--queued-orders))))))
                  (append (list (intern (plist-get recipe :package)))
                          recipe))))
  (setq parcel-cache-autoloads nil)
  (parcel-display-status-buffer)
  (parcel--queue-order order)
  (parcel--process-order (car parcel--queued-orders)))

(defun parcel--process-order (queued)
  "Process QUEUED order."
  (let ((order (cdr queued)))
    (unless (memq (parcel-order-status order) '(failed blocked))
      (parcel--run-next-build-step order))))

;;;###autoload
(defun parcel-queue-empty ()
  "Process all orders in `parcel--queued-orders'."
  (setq parcel--processed-order-count 0)
  (mapc #'parcel--process-order (reverse parcel--queued-orders)))

;;;###autoload
(defun parcel-delete-package (force &optional package)
  "Remove a PACKAGE from all caches and disk.
This also deletes its dependencies and will error if PACKAGE has dependents.
If FORCE is non-nil do not confirm before deleting."
  (interactive (list current-prefix-arg
                     (intern (completing-read "Delete package: "
                                              (mapcar #'car parcel--queued-orders)))))
  (when (or force (yes-or-no-p (format "Delete package %S?" package)))
    (if-let ((order (alist-get package parcel--queued-orders)))
        (let ((repo-dir      (parcel-order-repo-dir order))
              (build-dir     (parcel-order-build-dir order))
              (dependents    (parcel-order-dependents order))
              (dependencies  (parcel-order-dependencies order)))
          (if (cl-some (lambda (dependent)
                         (let ((item (intern (parcel-order-package dependent))))
                           (or (file-exists-p (parcel-order-repo-dir dependent))
                               (file-exists-p (parcel-order-build-dir dependent))
                               (alist-get item parcel--queued-orders)
                               (alist-get item parcel--order-cache))))
                       dependents)
              (user-error "Cannot delete %S unless dependents %S are deleted first" package
                          (mapcar #'parcel-order-package dependents))
            (when (file-exists-p repo-dir)  (delete-directory repo-dir  'recursive))
            (when (file-exists-p build-dir) (delete-directory build-dir 'recursive))
            (setq parcel--order-cache (cl-remove package parcel--order-cache :key #'parcel-order-package :test #'equal)
                  parcel--queued-orders (cl-remove package parcel--queued-orders :key #'car))
            (parcel--write-order-cache)
            (when (equal (buffer-name) parcel-status-buffer) (parcel-display-status-buffer))
            (message "Deleted package %S" package)
            (dolist (dependency dependencies)
              (parcel-delete-package 'force (intern (parcel-order-package dependency))))))
      (user-error "%S not a queued order" package))))

;;;###autoload
(defun parcel-rebuild-package (item &optional hide)
  "Rebuild ITEM's associated package.
If HIDE is non-nil, do not display `parcel-status-buffer'."
  (interactive
   (list (let ((item
                (completing-read "Rebuild package: "
                                 (sort (mapcar #'car parcel--queued-orders) #'string<)
                                 nil 'require-match)))
           (if (string-empty-p item)
               (user-error "No package selected")
             (intern item)))))
  (if-let ((queued (assoc item parcel--queued-orders)))
      (let ((order (cdr queued)))
        (parcel--update-order-info order "Rebuilding" 'rebuilding)
        (setq parcel-cache-autoloads nil)
        (setf (parcel-order-build-steps order)
              (cl-remove #'parcel--clone-dependencies
                         (copy-tree parcel-build-steps)))
        (setf (parcel-order-queue-time order) (current-time))
        (parcel--process-order queued)
        (unless hide
          (when-let ((buffer (get-buffer parcel-status-buffer)))
            (kill-buffer buffer))
          (parcel-display-status-buffer)))
    (user-error "Package %S has no queued order" item)))

;;;; STATUS BUFFER
(defvar parcel-status-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "I")  'parcel-status-mode-send-input)
    (define-key map (kbd "R")  'parcel-status-mode-visit-repo)
    (define-key map (kbd "B")  'parcel-status-mode-visit-build)
    (define-key map (kbd "D")  'parcel-status-delete-package)
    map))

(define-derived-mode parcel-status-mode tabulated-list-mode "Parcel Status Mode"
  "Mode for interacting with the parcel status buffer.

  \\{parcel-status-mode-map}")

(defun parcel-status-mode-send-input ()
  "Send input string to current process."
  (interactive)
  (if-let ((order (get-text-property (line-beginning-position) 'order))
           (process (parcel-order-process order))
           ((process-live-p process)))
      (let* ((input (read-string (format "Send input to %S: " (process-name process)))))
        (process-send-string process (concat input "\n")))
    (user-error "No running process associated with %S" (parcel-order-package order))))

(defun parcel-status-mode--visit (type)
  "Visit current order's TYPE dir.
TYPE is either the symbol `repo` or `build`."
  (if-let ((order (get-text-property (line-beginning-position) 'order))
           (dir   (funcall (intern (format "parcel-order-%s-dir" type)) order))
           ((file-exists-p dir)))
      (dired dir)
    (user-error "No %s dir associated with current line" type)))

(defun parcel-status-mode-visit-repo ()
  "Visit repo associated with current process."
  (interactive)
  (parcel-status-mode--visit 'repo))

(defun parcel-status-mode-visit-build ()
  "Visit builds dir associated with current process."
  (interactive)
  (parcel-status-mode--visit 'build))

(defun parcel-status-delete-package (&optional force)
  "Delete package at point. If FORCE is non-nil, do not confirm."
  (interactive)
  (if-let ((package (intern (parcel-order-package
                             (get-text-property (line-beginning-position) 'order)))))
      (parcel-delete-package force package)))

(provide 'parcel)
;;; parcel.el ends here
