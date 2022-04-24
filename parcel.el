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

(defvar parcel--order-info-timer nil "Timer to debounce order info printing.")
(defvar parcel--pre-built-steps
  '(parcel--queue-dependencies parcel--add-info-path parcel--activate-package)
  "List of steps for packages which are already built.")

(defcustom parcel-after-init-hook nil
  "Parcel's analogue to `after-init-hook'.
This is run after all orders queued during init have finished processing.
It is only run once after init.
Note a blocked process will prevent this hook from being run."
  :type 'hook)

(defcustom parcel-post-queue-hook nil
  "Hook run after a queue is finished processing.
Note blocked or failed orders will prevent this hook from being run."
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

(defcustom parcel-makeinfo-executable (executable-find "makeinfo")
  "Path of the makeinfo executable."
  :type 'string)

(defcustom parcel-install-info-executable
  (executable-find "install-info")
  "Path of the install-info executable."
  :type 'string)

(defcustom parcel-order-info-debounce-interval 0.02
  "Number of idle seconds to wait before printing order statuses.
Setting this to too low may cause the status buffer to block more.
Setting it too high causes prints fewer status updates."
  :type 'number)

(defcustom parcel--process-busy-interval 5
  "Seconds to wait between subprocess outputs before declaring process blocked."
  :type 'number)

(defcustom parcel-build-steps '(parcel--clone
                                parcel--add-remotes
                                parcel--checkout-ref
                                parcel--run-pre-build-commands
                                parcel--clone-dependencies
                                parcel--link-build-files
                                parcel--byte-compile
                                parcel--generate-autoloads-async
                                parcel--compile-info
                                parcel--install-info
                                parcel--add-info-path
                                parcel--run-post-build-commands
                                parcel--activate-package)
  "List of steps which are run when installing/building a package."
  :type 'list)

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

(defcustom parcel-order-functions '(parcel-order-defaults)
  "Abnormal hook run to alter orders.
Each element must be a unary function which accepts an order.
An order may be nil, a symbol naming a package, or a plist.
The function may return nil or a plist to be merged with the order.
This hook is run via `run-hook-with-args-until-success'."
  :type 'hook)

(defun parcel-recipe-defaults (order)
  "Default ORDER modifications. Matches any ORDER."
  (let ((plist))
    (unless (plist-get order :files)
      (push (list :defaults) plist)
      (push :files plist))
    (when-let ((url (plist-get order :url))
               ((string-match-p "depp.brause.cc" url)))
      (push nil plist)
      (push :depth plist))
    plist))

(defcustom parcel-recipe-functions '(parcel-recipe-defaults)
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
  '(emacs cl-lib cl-generic nadvice org org-mode map seq json parcel)
  "Ignore these unless the user explicitly requests they be installed.")

(defvar parcel-overriding-prompt nil "Overriding prompt for interactive functions.")

(defmacro parcel--read-file (path)
  "Read file at PATH into memory."
  (declare (debug all))
  (let ((psym (gensym "path")))
    `(when-let ((,psym ,path)
                (file (expand-file-name ,psym))
                ((file-exists-p file)))
       (condition-case err
           (with-temp-buffer
             (insert-file-contents file)
             (read (current-buffer)))
         ((error) (warn "Error reading %S into memory: %S" ,psym err))))))

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

(defun parcel--read-menu-cache ()
  "Read the menu-cache."
  (parcel--read-file (expand-file-name "menu-items.el" parcel-cache-directory)))

(defvar parcel-menu--candidates-cache
  (when parcel-cache-menu-items (parcel--read-menu-cache))
  "Cache for menu candidates.")

(defvar parcel--package-requires-regexp
  "\\(?:[[:space:]]*;+[[:space:]]*Package-Requires:[[:space:]]*\\(([^z-a]*?))\\)\\)"
  "Regexp matching the Package-Requires metadata in an elisp source file.")

(defvar parcel-recipe-keywords '( :branch :depth :fork :host :nonrecursive :package
                                  :post-build :pre-build :protocol :remote :repo)
  "Recognized parcel recipe keywords.")

(defvar parcel--queue-index 0 "Index for tracking current queue.")

(cl-defstruct (parcel-queue (:constructor parcel-queue-create)
                            (:type list)
                            (:named))
  "Queue to hold parcel orders."
  autoloads forms init (id (cl-incf parcel--queue-index))
  orders (processed 0) (status 'incomplete))

(defvar parcel--queues (list (parcel-queue-create :id parcel--queue-index))
  "List of parcel queue objects.")

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

(defun parcel--write-menu-cache ()
  "Write menu item cache to disk."
  (unless (file-exists-p parcel-cache-directory)
    (make-directory parcel-cache-directory))
  (parcel--write-file (expand-file-name "menu-items.el" parcel-cache-directory)
    (prin1 parcel-menu--candidates-cache)))

;;@TODO: allow passing in menu functions.
(defun parcel-menu--candidates (&optional recache)
  "Return alist of `parcel-menu-functions' candidates.
If RECACHE is non-nil, recompute `parcel-menu--candidates-cache'."
  (or (and (not recache) parcel-menu--candidates-cache)
      (prog1 (setq parcel-menu--candidates-cache
                   (sort (apply #'append
                                (copy-tree
                                 (cl-loop for fn in parcel-menu-functions
                                          ;; Allows adding a symbol prior menu installation.
                                          collect (and (fboundp fn) (funcall fn 'index)))))
                         (lambda (a b) (string-lessp (car a) (car b)))))
        (when parcel-cache-menu-items (parcel--write-menu-cache)))))

(defun parcel--current-queue ()
  "Return the current queue."
  (car (nthcdr (- (1- (length parcel--queues)) parcel--queue-index) parcel--queues)))

(defsubst parcel-alist-get (key alist)
  "Return KEY's value in ALIST.
Simplified version of `alist-get'."
  (cdr (assq key alist)))

;;@TODO: clean up interface.
;;;###autoload
(defun parcel-menu-item (&optional interactive symbol menus filter no-descriptions)
  "Return menu item matching SYMBOL in MENUS or `parcel-menu-functions'.
If SYMBOL is nil, prompt for it.
If INTERACTIVE is equivalent to \\[universal-argument] prompt for MENUS.
FILTER must be a function which accepts a candidate.
If it returns nil, the candidate is not considered for selection.
If NO-DESCRIPTIONS is non-nil, candidate descriptions are not included.
This is faster (what you want with non-interactive calls)."
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
         (candidates
          (let ((c (if filter
                       (cl-remove-if-not filter (parcel-menu--candidates))
                     (parcel-menu--candidates))))
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
                             (completing-read (or parcel-overriding-prompt "Package: ")
                                              candidates nil t)))
                        (if no-descriptions
                            choice
                          (car (split-string choice "\\(?:[[:space:]]+\\)")))))))
         (candidate (parcel-alist-get symbol
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
      (let ((menu-item (parcel-menu-item nil item nil nil (not interactive))))
        (unless menu-item (user-error "No menu-item for %S" item))
        (when parcel-order-functions
          (push (run-hook-with-args-until-success 'parcel-order-functions item)
                ingredients))
        (push menu-item ingredients)))
     ((listp item)
      (setq package (pop item))
      (unless (parcel--inheritance-disabled-p item)
        (when-let ((parcel-order-functions)
                   (mods (run-hook-with-args-until-success 'parcel-order-functions item)))
          (push mods ingredients)
          (when (or (plist-get item :inherit) (plist-get mods :inherit))
            (push (parcel-menu-item nil package nil nil 'no-descriptions) ingredients))))
      (push item ingredients)
      (setq ingredients (nreverse ingredients)))
     (t (signal 'wrong-type-argument `((null symbolp listp) . ,item))))
    (if-let ((recipe (apply #'parcel-merge-plists ingredients)))
        (progn
          (unless (plist-get recipe :package)
            (setq recipe (plist-put recipe :package (format "%S" package))))
          (when-let ((parcel-recipe-functions)
                     (recipe-mods (run-hook-with-args-until-success
                                   'parcel-recipe-functions recipe)))
            (setq recipe (parcel-merge-plists recipe recipe-mods)))
          (if interactive
              (progn
                (kill-new (format "%S" recipe))
                (message "%S recipe copied to kill-ring:\n%S" (plist-get recipe :package) recipe))
            recipe))
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
  (cl-destructuring-bind
      (&key url local-repo (repo url) fetcher (host fetcher) &allow-other-keys)
      recipe
    (expand-file-name
     (if (parcel--full-repo-protocol-p repo)
         (progn
           (require 'url-parse)
           (let ((url (url-generic-parse-url repo)))
             (format "%s._.%s" ; repo._.host
                     (or local-repo
                         (file-name-sans-extension
                          (replace-regexp-in-string
                           ".*/" "" (url-filename
                                     (url-generic-parse-url (or local-repo repo))))))
                     (url-host url))))
       ;;repo-or-local-repo.user.host
       (format "%s.%s.%s"
               (or local-repo (parcel--repo-name repo))
               (parcel--repo-user repo)
               (symbol-name host)))
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
  "Order for queued processing."
  build-dir build-steps dependencies dependents files includes init log package
  process queue-time recipe repo-dir statuses queue-id)

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

(defun parcel--queued-orders (&optional arg)
  "Return list of queued orders from ARG.
If ARG is a non-negative integer return Nth queue's orders in `parcel--queues'.
Otherwise return a list of all queued orders."
  (if arg
      (parcel-queue-orders (car (nthcdr (- (1- (length parcel--queues)) parcel--queue-index)
                                        parcel--queues)))
    (apply #'append (nreverse (cl-loop for queue in parcel--queues
                                       collect (parcel-queue-orders queue))))))

(defsubst parcel--status-face (status &optional default)
  "Return face for STATUS or DEFAULT if not found."
  (cond
   ((eq status 'blocked)  'parcel-blocked)
   ((eq status 'finished) 'parcel-finished)
   ((eq status 'failed)   'parcel-failed)
   (t                     (or default 'default))))

(defun parcel--events (&rest packages)
  "Return sorted event log string for PACKAGES.
If PACKAGES is nil, use all available orders."
  (let* ((packages (delete-dups (if packages (mapcar #'intern packages))))
         (queued (if packages
                     (cl-remove-if-not (lambda (cell) (member (car cell) packages))
                                       (parcel--queued-orders))
                   (parcel--queued-orders)))
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
                                                   (parcel--status-face status '(:weight bold)))))
                          (push (cons time
                                      (format "[%s]%s %s"
                                              (format-time-string
                                               "%02s.%3N" (time-subtract time parcel--order-queue-start-time))
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

(defun parcel-status-buffer-entries (&optional queued)
  "Return list of `tabultaed-list-entries' from QUEUED orders."
  (cl-loop for (item . order) in (or queued (parcel-queue-orders (parcel--current-queue)))
           for status = (parcel-order-status order)
           collect
           (list item (vector (propertize (parcel-order-package order)
                                          'face (parcel--status-face status)
                                          'order order)
                              (symbol-name status)
                              (parcel-order-info order)))))

(defun parcel--print-order-status (&optional queued)
  "Print status of QUEUED orders in `parcel-status-buffer'."
  (let ((buffer (get-buffer parcel-status-buffer)))
    (unless buffer (parcel--initialize-process-buffer queued))
    (with-current-buffer (get-buffer-create parcel-status-buffer)
      (setq tabulated-list-entries (parcel-status-buffer-entries queued))
      (tabulated-list-init-header)
      (tabulated-list-print 'remember-pos 'update)
      (parcel--set-header-line queued))))

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
    (prin1 (cl-loop for (_ . order) in (parcel--queued-orders)
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
  (mapcar #'parcel--cache-entry-to-order
          (parcel--read-file (expand-file-name "cache.el" parcel-cache-directory))))

(defvar parcel--order-cache (when parcel-cache-orders (parcel--read-order-cache))
  "Built package information cache.")

(defsubst parcel--run-next-build-step (order)
  "Run ORDER's next build step with ARGS."
  (funcall (or (pop (parcel-order-build-steps order)) #'parcel--finalize-order) order))

(defun parcel--continue-mono-repo-dependency (order)
  "Continue processing ORDER after it's mono-repo is in the proper state."
  (unless (memq (parcel-order-statuses order) '(finished build-linked))
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
    (when (memq status '(finished failed blocked))
      (mapc #'parcel--order-check-status (parcel-order-dependents order)))
    (when (eq status 'ref-checked-out)
      (mapc #'parcel--continue-mono-repo-dependency (parcel-order-includes order))))
  (when info (parcel--log-event order info))
  (let ((status-buffer-visible-p (get-buffer-window parcel-status-buffer t)))
    (if status-buffer-visible-p
        (progn
          (when parcel--order-info-timer (cancel-timer parcel--order-info-timer))
          (setq parcel--order-info-timer
                (run-at-time parcel-order-info-debounce-interval
                             nil #'parcel--print-order-status)))
      (when (eq status 'failed) (parcel-display-status-buffer)))))

(defun parcel--log-duration (order)
  "Return ORDER's log duration."
  ;;most recent event is car of log
  (let* ((log (parcel-order-log order))
         (end (nth 1 (car log))))
    (time-subtract end (parcel-order-queue-time order))))

(defun parcel--finalize-queue (queue)
  "Run QUEUE's post isntallation functions:
- load cached autoloads
- evaluate deferred package configuration forms
- possibly run `parcel-after-init-hook'."
  (when-let ((autoloads (parcel-queue-autoloads queue)))
    (eval `(progn ,@autoloads) t))
  (when-let ((forms (parcel-queue-forms queue)))
    (eval `(progn ,@(apply #'append (nreverse forms))) t))
  (setf (parcel-queue-status queue) 'complete)
  (if (and (parcel-queue-init queue) (= (parcel-queue-id queue) (1- (length parcel--queues))))
      (progn
        (run-hooks 'parcel-after-init-hook)
        (parcel-split-queue))
    (cl-incf parcel--queue-index)
    (when-let ((queue (parcel--current-queue)))
      (parcel--process-queue queue))))

(defun parcel--finalize-order (order)
  "Declare ORDER finished or failed."
  (let ((status (parcel-order-status order)))
    (if (eq  status 'finished)
        (cl-loop for order in (parcel-order-dependents order)
                 unless (eq (parcel-order-status order) 'finished)
                 do (parcel--order-check-status order))
      (unless (eq (parcel-order-status order) 'failed)
        (parcel--update-order-info
         order
         (concat  "✓ " (format-time-string "%s.%3N" (parcel--log-duration order)) " secs")
         'finished)
        (when-let ((queue (nth parcel--queue-index (reverse parcel--queues)))
                   ((= (cl-incf (parcel-queue-processed queue))
                       (length (parcel-queue-orders queue)))))
          (parcel--finalize-queue queue))))))

(defun parcel--queue-order (item &optional status)
  "Queue (ITEM . ORDER) in `parcel--queued-orders'.
If STATUS is non-nil, the order is given that initial status.
RETURNS order structure."
  (if (and (not after-init-time) (parcel-alist-get item (parcel--queued-orders)))
      (warn "%S already queued. Duplicate?" item)
    (let* ((status  (or status 'queued))
           (info    "Package queued")
           (package (if (listp item) (car item) item))
           (name    (format "%S" package))
           (recipe  (condition-case err
                        (parcel-recipe item)
                      ((error) (setq status 'failed info (format "No recipe: %S" err))
                       nil)))
           (repo-dir (when recipe
                       (condition-case err
                           (parcel-repo-dir recipe)
                         ((error)
                          (setq status 'failed
                                info (format "Unable to determine repo dir: %S" err))))))
           (build-dir (when recipe (parcel-build-dir recipe)))
           (cached    (cl-some (lambda (o) (when (equal (parcel-order-package o) name) o))
                               parcel--order-cache))
           (built-p    nil)
           (order
            (parcel-order-create
             :package name      :recipe recipe       :statuses (list status)
             :repo-dir repo-dir :build-dir build-dir :queue-time (current-time)
             :init (not after-init-time)
             :build-steps
             (when recipe
               (if (file-exists-p build-dir)
                   (progn (setq built-p t) parcel--pre-built-steps)
                 (when-let  ((steps (copy-tree parcel-build-steps)))
                   (when (file-exists-p repo-dir)
                     (setq steps (delq 'parcel--clone steps))
                     (when-let ((cached)
                                (cached-recipe (parcel-order-recipe cached))
                                ((eq recipe cached-recipe)))
                       (setq steps (cl-set-difference steps '(parcel--add-remotes
                                                              parcel--checkout-ref
                                                              parcel--dispatch-build-commands)))))
                   steps)))))
           (mono-repo
            (unless built-p
              (cl-some (lambda (cell)
                         (when-let ((queued (cdr cell))
                                    ((and repo-dir
                                          (equal repo-dir (parcel-order-repo-dir queued)))))
                           queued))
                       (reverse (parcel--queued-orders))))))
      (prog1 order
        (push (cons package order) (parcel-queue-orders (parcel--current-queue)))
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

;;@FIX: when a folder is linked over we're nesting it in itself.
;; e.g. slime/contrib becomes slime/contrib/contrib/
(defun parcel--files (order &optional files nocons)
  "Return alist of ORDER :files to be symlinked: (PATH . TARGET PATH).
FILES and NOCONS are used recursively."
  (let* ((repo-dir          (parcel-order-repo-dir order))
         (default-directory repo-dir)
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
        ;;@FIX: subdir needn't be same name as globbed path...
        (`(,_subdir . ,paths)
         (cl-loop for path in paths
                  for globbed = (file-expand-wildcards path)
                  do (cl-loop for p in globbed
                              do (push (cons (expand-file-name p repo-dir)
                                             (let ((default-directory build-dir))
                                               (expand-file-name p build-dir)))
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

(defun parcel--process-busy (process)
  "Update order status when PROCESS has stopped producing output."
  (when-let (((eq (process-status process) 'run))
             (order (process-get process :order)))
    (parcel--update-order-info order (process-get process :result) 'blocked)))

(defun parcel--process-filter (process output &optional pattern status)
  "Filter PROCESS OUTPUT.
PATTERN is a string which is checked against the entire process output.
If it matches, the order associated with process has its STATUS updated."
  (process-put process :raw-output (concat (process-get process :raw-output) output))
  (let* ((order  (process-get process :order))
         (result (process-get process :result))
         (timer  (process-get process :timer))
         (lines  (split-string (concat result output) parcel-process-newline-regexp))
         (last-is-full-line-p (string-empty-p (car (last lines)))))
    (when timer (cancel-timer timer))
    (process-put process :timer (run-at-time parcel--process-busy-interval nil
                                             (lambda () (parcel--process-busy process))))
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
  (if-let ((files
            (cl-loop for (repo-file . build-file) in
                     (or (parcel-order-files order)
                         (setf (parcel-order-files order) (parcel--files order)))
                     for f = (when-let (((string-match-p "\\.texi\\(nfo\\)?$" repo-file))
                                        (info (concat (file-name-sans-extension build-file) ".info"))
                                        ((not (file-exists-p info))))
                               (list repo-file "-o" info))
                     when f collect f))
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
             ((not (file-exists-p dir))))
    (cl-loop for (repo-file . build-file) in
             (or (parcel-order-files order)
                 (setf (parcel-order-files order) (parcel--files order)))
             for f = (cond
                      ((string-match-p "\\.info$" build-file) build-file)
                      ((string-match-p "\\.texi\\(nfo\\)?$" repo-file)
                       (concat (file-name-sans-extension build-file) ".info")))
             when (and f (file-exists-p f))
             do (parcel-with-process
                    (parcel-process-call parcel-install-info-executable f dir)
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
                ;; Replace comment delimiters in multi-line package-requires metadata.
                (read (replace-regexp-in-string ";" "" (match-string 1)))
              (error "Unable to parse %S Package-Requires metadata: %S" main err))))))))

(defun parcel--queue-dependencies (order)
  "Queue ORDER's dependencies."
  (parcel--update-order-info order "Queueing Dependencies" 'queueing-deps)
  (let* ((recipe       (parcel-order-recipe order))
         (dependencies (parcel--dependencies recipe))
         (externals    (cl-loop for spec in dependencies
                                unless (memq (car spec) parcel-ignored-dependencies)
                                collect spec))
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
                   (queued     (parcel-alist-get dependency (parcel--queued-orders)))
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
                                        (memq dependency parcel-ignored-dependencies))
                                      dependencies :key #'car))))
    (if (and emacs (version< emacs-version (cadr emacs)))
        (parcel--update-order-info
         order (format "Requires %S; running %S" emacs emacs-version) 'failed)
      (if externals
          ;;@TODO: Major Version conflict checks?
          (let ((finished 0))
            (dolist (spec externals)
              (let* ((dependency (car spec))
                     (queued     (parcel-alist-get dependency (parcel--queued-orders)))
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
      (cl-destructuring-bind ( &key remotes ref tag &allow-other-keys
                               &aux
                               (remote (if (stringp remotes) remotes (car remotes)))
                               (branch (or (plist-get (cdr-safe (car-safe remotes)) :branch)
                                           (plist-get recipe :branch)))
                               (target (or ref tag branch)))
          recipe
        (when target
          (parcel-with-process
              (apply #'parcel-process-call
                     `("git"
                       ,@(delq nil
                               (cond
                                (ref    (list "checkout" ref))
                                (tag    (list "checkout" (concat ".git/refs/tags/" tag)))
                                (branch (list "switch" "-C" branch
                                              (format "%s/%s" (if (listp remote)
                                                                  (car remote)
                                                                remote)
                                                      branch)))))))
            (unless success
              (parcel--update-order-info
               order (format "Unable to check out ref: %S " (string-trim stderr))
               'failed))))
        (unless (eq (parcel-order-status order) 'failed)
          (parcel--update-order-info
           order (if target (format "%S ref checked out" target) "Default ref checked out") 'ref-checked-out)
          (parcel--run-next-build-step order))))))

(defun parcel--checkout-ref (order)
  "Checkout ORDER's :ref.
The :branch and :tag keywords are syntatic sugar and are handled here, too."
  (parcel--update-order-info order "Checking out repo ref")
  (let* ((default-directory (parcel-order-repo-dir order))
         (package           (parcel-order-package order))
         (recipe            (parcel-order-recipe order))
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
             :name     (format "parcel-fetch-%s" package)
             :command  '("git" "fetch" "--all")
             :filter   (lambda (process output)
                         (parcel--process-filter process output "fatal" 'failed))
             :sentinel #'parcel--checkout-ref-process-sentinel)))
      (process-put process :order order)
      (setf (parcel-order-process order) process))))

(defun parcel--set-header-line (queued)
  "Set `parcel-buffer' header line to reflect QUEUED order statuses."
  (let* ((queued (or queued (parcel-queue-orders (parcel--current-queue))))
         (counts nil)
         (queue-len (length queued)))
    (dolist (q queued)
      (let ((status (parcel-order-status (cdr q))))
        (if (parcel-alist-get status counts)
            ;; Avoid `parcel-alist-get'. doesn't return PLACE.
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
                     (if-let ((finished (parcel-alist-get 'finished counts)))
                         (* (/ (float finished) queue-len) 100)
                       0.00)
                     (or (parcel-alist-get 'finished counts) 0)
                     (propertize "Blocked" 'face 'parcel-blocked)
                     (or (parcel-alist-get 'blocked  counts) 0)
                     (propertize "Failed" 'face 'parcel-failed)
                     (or (parcel-alist-get 'failed   counts) 0)))))))

(defun parcel-status-buffer-line (order)
  "Return status string for ORDER."
  (let* ((package (parcel-order-package order))
         (status  (parcel-order-status  order))
         (name    (format "%-30s"
                          (propertize package 'face
                                      (parcel--status-face status '(:weight bold))))))
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
    (let (failed blocked)
      (cl-loop for dep-order in (parcel-order-dependencies order)
               for status = (parcel-order-status dep-order)
               unless (eq status 'finished)
               do (if (eq status 'failed)
                      (push (parcel-order-package dep-order) failed)
                    (push (parcel-order-package dep-order) blocked)))
      (cond
       (failed (parcel--update-order-info
                order (format "Failed dependencies: %S" failed) 'failed))
       (blocked (parcel--update-order-info
                 order (format "Blocked by dependencies: %S" blocked) 'blocked))
       (t (parcel--run-next-build-step order))))))

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
                        (parcel--process-filter
                         process output
                         "\\(?:^\\(?:Password\\|Username\\|passphrase\\)\\)" 'blocked))
            :sentinel #'parcel--clone-process-sentinel)))
      (process-put process :order order)
      (setf (parcel-order-process order) process))))

(defun parcel-generate-autoloads (package dir)
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
                  (parcel-queue-autoloads (parcel--current-queue))))
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
         (dependency-dirs
          (cl-loop for item in (parcel-dependencies (intern (parcel-order-package order))
                                                    parcel-ignored-dependencies)
                   when item
                   for build-dir = (parcel-order-build-dir (parcel-alist-get item (parcel--queued-orders)))
                   when build-dir
                   collect build-dir))
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

(defun parcel-dependencies (item &optional ignore recurse)
  "Return recursive list of ITEM's dependencies.
IGNORE may be a list of symbols which are not included in the resulting list.
RECURSE is used to track recursive calls."
  (if-let ((order (parcel-alist-get item (parcel--queued-orders)))
           (dependencies (parcel--dependencies (parcel-order-recipe order))))
      (let ((transitives (cl-loop for dependency in dependencies
                                  for name = (car dependency)
                                  unless (memq name ignore)
                                  collect
                                  (cons name (parcel-dependencies name ignore 'recurse)))))
        (delete-dups (flatten-tree transitives)))
    (when recurse item)))

(defun parcel-dependents (item &optional recurse)
  "Return recursive list of packages which depend on ITEM.
RECURSE is used to keep track of recursive calls."
  (if-let ((order (parcel-alist-get item (parcel--queued-orders)))
           (dependents (parcel-order-dependents order)))
      (let ((transitives (cl-loop for dependent in dependents
                                  collect
                                  (let ((i (intern (parcel-order-package dependent))))
                                    (cons i (parcel-dependents i 'recurse))))))
        (delete-dups (nreverse (flatten-tree transitives))))
    (when recurse item)))

;;;; COMMANDS/MACROS
(defun parcel-print-log (&rest packages)
  "Print log for PACKAGES."
  (interactive (completing-read-multiple "Log for Packages: "
                                         (mapcar #'car (parcel--queued-orders))))
  (let ((queued (if packages
                    (cl-remove-if-not (lambda (package)
                                        (parcel-alist-get (intern package) (parcel--queued-orders)))
                                      packages)
                  (mapcar (lambda (queued) (symbol-name (car queued)))
                          (parcel--queued-orders)))))
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

(defun parcel--initialize-process-buffer (&optional queued display)
  "Initialize the parcel process buffer for QUEUED orders.
If QUEUED is nil, display the most recent queue's orders.
When DISPLAY is non-nil, display the buffer."
  (with-current-buffer (get-buffer-create parcel-status-buffer)
    (unless (derived-mode-p 'parcel-status-mode) (parcel-status-mode))
    (with-silent-modifications (erase-buffer))
    (setq queued
          (or queued (parcel-queue-orders (parcel--current-queue)))
          tabulated-list-use-header-line nil
          tabulated-list-format parcel-status-buffer-list-format
          tabulated-list-entries (parcel-status-buffer-entries queued))
    (tabulated-list-init-header)
    (tabulated-list-print-fake-header)
    (tabulated-list-print 'remember-pos 'update)
    (parcel--set-header-line queued)
    (when display (pop-to-buffer-same-window (current-buffer)))))

;;;###autoload
(defun parcel-display-status-buffer (&optional arg)
  "Diplay `parcel-status-buffer' for latest queue.
When ARG is non-nil display all queues, else, display only the most recent."
  (interactive "P")
  ;; If this is invoked during init, we want it to be updated.
  (parcel--initialize-process-buffer
   (when arg (apply #'append (cl-loop for queue in parcel--queues
                                      collect (parcel-queue-orders queue))))
   'display))

;;;###autoload
(defmacro parcel (order &rest body)
  "Install ORDER, then execute BODY.
If ORDER is `nil`, defer BODY until orders have been processed."
  (declare (indent 1))
  `(progn
     ,@(unless (null order) (list `(parcel--queue-order ,order)))
     ,@(when body (list `(push ',body (parcel-queue-forms (parcel--current-queue)))))))

;;;###autoload
(defmacro parcel-use-package (order &rest body)
  "Execute BODY in `use-package' declartion after ORDER is finished.
If the :disabled keyword is present in body, the package is completely ignored.
This happens regardless of the value associated with :disabled.
The expansion is a string indicating the package has been disabled."
  (declare (indent 1))
  (if (memq :disabled body)
      (format "%S :disabled by parcel-use-package" order)
    `(parcel ',order
       (use-package ,(if (listp order) (car order) order)
         ,@body))))

;;;###autoload
(defun parcel-try-package (&rest orders)
  "Try ORDERS.
Install the repo/build files on disk.
Activate the corresponding package for the current session.
ORDER's package is not made available during subsequent sessions."
  (interactive (list
                (let ((recipe (parcel-menu-item
                               nil nil nil
                               (lambda (candidate)
                                 (not (parcel-alist-get (car candidate)
                                                        (parcel--queued-orders)))))))
                  (append (list (intern (plist-get recipe :package)))
                          recipe))))
  (setq parcel-cache-autoloads nil)
  (parcel-display-status-buffer)
  (dolist (order orders)
    ;;@FIX: wasteful to pad out the order to make it QUEUED.
    (let ((package (if (listp order) (car order) order)))
      (parcel--process-order (cons package (parcel--queue-order order))))))

(defun parcel--process-order (queued)
  "Process QUEUED order."
  (let ((order (cdr queued)))
    (unless (memq (parcel-order-status order) '(failed blocked))
      (parcel--run-next-build-step order))))

(defun parcel--process-queue (queue)
  "Process  orders in QUEUE."
  (mapc #'parcel--process-order (reverse (parcel-queue-orders queue)))
  (run-hooks 'parcel-post-queue-hook))

;;;###autoload
(defun parcel-process-init ()
  "Process entire queue."
  (setq parcel--queue-index 0)
  (parcel--process-queue (parcel--current-queue)))

;;;###autoload
(defun parcel-split-queue ()
  "Split remaining orders into new queue."
  (push (parcel-queue-create :init (not after-init-time))
        parcel--queues))

(defun parcel--order-on-disk-p (item)
  "Return t if ITEM has an associated order and a build or repo dir on disk."
  (when-let ((order (parcel-alist-get item (parcel--queued-orders))))
    (or (file-exists-p (parcel-order-repo-dir order))
        (file-exists-p (parcel-order-build-dir order)))))
;;@INCOMPLETE: We need to determine policy for deleting dependencies.
;; Maybe skip dependencies which weren't declared or dependencies of a declared order.

;;;###autoload
(defun parcel-delete-package (force with-deps &optional package asker)
  "Remove a PACKAGE from all caches and disk.
If WITH-DEPS is non-nil dependencies other than ASKER are deleted.
If FORCE is non-nil do not confirm before deleting."
  (when (or force (yes-or-no-p (format "Delete package %S?" package)))
    (if-let ((order (parcel-alist-get package (parcel--queued-orders))))
        (let ((repo-dir      (parcel-order-repo-dir  order))
              (build-dir     (parcel-order-build-dir order))
              (dependents    (delq asker (parcel-dependents package)))
              (dependencies  (when with-deps
                               (parcel-dependencies package parcel-ignored-dependencies))))
          (if (cl-some #'parcel--order-on-disk-p dependents)
              (message "Cannot delete %S unless dependents %S are deleted first"
                       package dependents)
            (when (file-exists-p repo-dir)  (delete-directory repo-dir  'recursive))
            (when (file-exists-p build-dir) (delete-directory build-dir 'recursive))
            (setq parcel--order-cache
                  (cl-remove package parcel--order-cache
                             :key #'parcel-order-package :test #'equal))
            (dolist (queue parcel--queues)
              (setf (parcel-queue-orders queue)
                    (cl-remove package (parcel-queue-orders queue) :key #'car)))
            (parcel--write-order-cache)
            (when (equal (buffer-name) parcel-status-buffer) (parcel-display-status-buffer))
            (message "Deleted package %S" package)
            (when with-deps
              (dolist (dependency dependencies)
                (parcel-delete-package 'force with-deps dependency package)))))
      (if-let ((recipe (parcel-recipe package)))
          (progn
            (when-let ((repo-dir (parcel-repo-dir recipe))) (delete-directory repo-dir 'recursive))
            (when-let ((build-dir (parcel-build-dir recipe))) (delete-directory build-dir 'recursive)))
        (user-error "%S not a queued order" package)))))

;;;###autoload
(defun parcel-rebuild-package (item &optional hide)
  "Rebuild ITEM's associated package.
If HIDE is non-nil, do not display `parcel-status-buffer'."
  (interactive
   (list (let ((item
                (completing-read "Rebuild package: "
                                 (sort (mapcar #'car (parcel--queued-orders)) #'string<)
                                 nil 'require-match)))
           (if (string-empty-p item)
               (user-error "No package selected")
             (intern item)))))
  (if-let ((queued (assoc item (parcel--queued-orders))))
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

(defun parcel--log-updates-process-sentinel (process event)
  "Handle PROCESS EVENT."
  (when (equal event "finished\n")
    (let ((order (process-get process :order)))
      (parcel--update-order-info order "End Update log" 'updates-logged)
      (parcel--run-next-build-step order))))

;;@INCOMPLETE:
;; What do we actually want to log here?
;; If the user is on a different branch which has an upstream?
;; Or do we strictly log the difference between the recipe's declared ref and upstream?
;; Probably the latter, because that's the only case we can automatically update.
;; Anything else will require user intervention. ~ NV [2022-03-03]
(defun parcel--log-updates (order)
  "Log ORDER's fetched commits."
  (parcel--update-order-info order "Start Update log" 'log-updates)
  (let* ((default-directory (parcel-order-repo-dir order))
         (recipe (parcel-order-recipe order))
         (remotes (plist-get recipe :remotes))
         (remote (if (listp remotes) (car remotes) remotes))
         (process (make-process
                   :name (format "parcel-log-updates-%s" (parcel-order-package order))
                   ;; Pager will break this process. Complains about terminal functionality.
                   :command (list "git" "--no-pager" "log"
                                  (if (listp remote) (car remote) remote)
                                  "..." "HEAD")
                   :filter   #'parcel--process-filter
                   :sentinel #'parcel--log-updates-process-sentinel)))
    (process-put process :order order)))

(defun parcel--fetch-process-sentinel (process event)
  "Handle PROCESS EVENT."
  (when (equal event "finished\n")
    (let ((order (process-get process :order)))
      (parcel--update-order-info order "Updates fetched" 'updates-fetched)
      (parcel--run-next-build-step order))))

(defun parcel--fetch (order)
  "Fetch ORDER's remote's commits."
  (let ((default-directory (parcel-order-repo-dir order))
        (process (make-process
                  :name (format "parcel-fetch-update-%s" (parcel-order-package order))
                  :command  '("git" "fetch" "--all")
                  :filter   #'parcel--process-filter
                  :sentinel #'parcel--fetch-process-sentinel)))
    (process-put process :order order)))

;;;###autoload
(defun parcel-fetch (item &optional hide)
  "Fetch ITEM's associated package remote commits.
This does not merge changes or rebuild the packages.
If HIDE is non-nil don't display `parcel-status-buffer'."
  (interactive
   (list (let ((item
                (completing-read "Fetch updates: "
                                 (sort (mapcar #'car (parcel--queued-orders)) #'string<)
                                 nil 'require-match)))
           (if (string-empty-p item)
               (user-error "No package selected")
             (intern item)))))
  (if-let ((queued (assoc item (parcel--queued-orders))))
      (let ((order (cdr queued)))
        (parcel--update-order-info order "Fetching updates" 'fetching-updates)
        (setf (parcel-order-build-steps order) (list #'parcel--fetch #'parcel--log-updates))
        (setf (parcel-order-queue-time order) (current-time))
        (parcel--process-order queued)
        (unless hide
          (when-let ((buffer (get-buffer parcel-status-buffer)))
            (kill-buffer buffer))
          (parcel-display-status-buffer)
          (with-current-buffer parcel-status-buffer
            (goto-char (point-min))
            (re-search-forward (format "^%s " item) nil 'noerror))))
    (user-error "Package %S has no queued order" item)))

;;;###autoload
(defun parcel-fetch-all (&optional hide)
  "Fetch remote commits for every queued order.
If HIDE is non-nil, do not show `parcel-status-buffer'."
  (interactive "P")
  (mapc (lambda (queued) (parcel-fetch (car queued) hide)) (parcel--queued-orders)))

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
      (parcel-delete-package force nil package)))

;;; Lockfiles
(defun parcel-declared-p (item)
  "Return t if ITEM is declared in user's init file, nil otherwise."
  (when-let ((order (parcel-alist-get item (parcel--queued-orders))))
    (or (parcel-order-init order)
        (cl-loop for dependent in (parcel-dependents item)
                 when (parcel-alist-get dependent (parcel--queued-orders))
                 return t))))

(defun parcel-installed-p (item)
  "Return t if ITEM's associated repo directory is on disk, nil otherwise."
  (when-let ((order (parcel-alist-get item (parcel--queued-orders)))
             ((file-exists-p (parcel-order-repo-dir order))))
    t))

(defun parcel-worktree-dirty-p (item)
  "Return t if ITEM's associated repository has a dirty worktree, nil otherwise."
  (when-let ((order (parcel-alist-get item (parcel--queued-orders)))
             (recipe (parcel-order-recipe order))
             (repo-dir (parcel-order-repo-dir order))
             ((file-exists-p repo-dir))
             (default-directory repo-dir))
    (not (string-empty-p
          (parcel-process-output "git" "-c" "status.branch=false" "status" "--short")))))

(defun parcel-load-lockfile (&optional lockfile _force)
  "Load LOCKFILE.
If FORCE is non-nil,."
  (interactive "fLockfile: ")
  (message "%S" lockfile))

(defun parcel-write-lockfile (&optional path)
  "Write lockfile to PATH for current state of package repositories."
  (interactive "FWrite lockfile to: ")
  (unless path (user-error "Need file PATH"))
  (let* ((seen)
         (revisions
          (nreverse
           (apply
            #'append
            (cl-loop for queue in  parcel--queues
                     collect
                     (cl-loop for (item . order) in (parcel-queue-orders queue)
                              unless (member item seen)
                              for rev =
                              (let ((default-directory (parcel-order-repo-dir order)))
                                (parcel-with-process (parcel-process-call "git"
                                                                          "rev-parse"
                                                                          "HEAD")
                                  (when success (string-trim stdout))))
                              when rev
                              collect (cons item rev)
                              do (push item seen)))))))
    (parcel--write-file path (pp revisions))))

(provide 'parcel)
;;; parcel.el ends here
