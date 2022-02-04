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
(require 'cl-lib)
(require 'parcel-process)

(declare-function autoload-rubric "autoload")
(declare-function url-filename    "url-parse")
(declare-function url-host        "url-parse")
(defvar autoload-timestamps)

(defgroup parcel nil
  "An elisp package manager."
  :group 'parcel
  :prefix "parcel-")

(defface parcel-finished
  '((t (:weight bold :foreground "#00FF00")))
  "Indicates an order is finished.")

(defface parcel-blocked
  '((t (:weight bold :foreground "#EA00CC")))
  "Indicates an order is blocked.")

(defface parcel-failed
  '((t (:weight bold :foreground "#FF0000")))
  "Indicates an order has failed.")

(defcustom parcel-directory (expand-file-name "parcel" user-emacs-directory)
  "Location of the parcel package store."
  :type 'directory)

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
  (list :protocol 'https :remotes "origin" :inherit t :depth 1
        :build (list #'parcel--byte-compile #'parcel--generate-autoloads-async)))

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

(defvar parcel--build-functions nil
  "Abnormal hook run with recipes :build functions.")

(defvar parcel-ignored-dependencies
  (list 'emacs 'cl-lib 'cl-generic 'esxml 'nadvice 'org 'org-mode 'map 'seq)
  "Built in packages.
Ignore these unless the user explicitly requests they be installed.")
(defvar parcel-overriding-prompt nil "Overriding prompt for interactive functions.")
(defvar parcel-menu--candidates-cache nil "Cache for menu candidates.")
(defvar parcel--package-requires-regexp
  "\\(?:^;+[[:space:]]*Package-Requires[[:space:]]*:[[:space:]]*\\([^z-a]*?$\\)\\)"
  "Regexp matching the Package-Requires metadata in an elisp source file.")
(defvar parcel-recipe-keywords (list :pre-build :branch :depth :fork :host
                                     :nonrecursive :package :protocol :remote :repo)
  "Recognized parcel recipe keywords.")
(defvar parcel--queued-orders nil "List of queued orders.")
(defconst parcel-status-buffer "*Parcel*")

(defvar parcel--order-index -1 "Index used to track queued orders.")
(defvar parcel--order-queue-start-time nil
  "Time used to keep order logs relative to start of queue.")
(cl-defstruct parcel-order
  "Order object for queued processing."
  (package      nil :type string)
  (recipe       nil :type list)
  (steps        nil :type list)
  (status       nil :type string)
  (statuses     nil :type list)
  (dependencies nil :type list)
  (dependents   nil :type list)
  (index        (cl-incf parcel--order-index))
  (includes     nil)
  (repo-dir     nil)
  (build-dir    nil)
  (info         nil)
  (process      nil)
  (log          nil))

(define-minor-mode parcel-dev-mode "Just a way to toggle a variable for dev code.")

(defun parcel-merge-plists (&rest plists)
  "Return plist with set of unique keys from PLISTS.
Values for each key are that of the right-most plist containing that key."
  (let ((plists (delq nil plists))
        current plist)
    (while (setq current (pop plists))
      (while current (setq plist (plist-put plist (pop current) (pop current)))))
    plist))

(defun parcel-clean-plist (plist)
  "Return PLIST copy sans keys which are not members of `parcel-recipe-keywords'."
  (apply #'append (cl-loop for key in parcel-recipe-keywords
                           for member = (plist-member plist key)
                           collect (when member
                                     (cl-subseq (plist-member plist key) 0 2)))))

(defun parcel-menu--candidates ()
  "Return alist of `parcel-menu-functions' candidates."
  (or parcel-menu--candidates-cache
      (setq parcel-menu--candidates-cache
            (sort (apply #'append
                         (cl-loop for fn in parcel-menu-functions
                                  for index = (funcall fn 'index)
                                  when index collect index))
                  (lambda (a b) (string-lessp (car a) (car b)))))))

;;@TODO: clean up interface.
;;;###autoload
(defun parcel-menu-item (&optional interactive symbol menus)
  "Return menu item matching SYMBOL in MENUS or `parcel-menu-functions'.
If SYMBOL is nil, prompt for it.
If INTERACTIVE is equivalent to \\[universal-argument] prompt for MENUS."
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
         (candidates (parcel-menu--candidates))
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

(defun parcel--log-event (order text)
  "Store TEXT in ORDER's log."
  (let ((log (parcel-order-log order)))
    (setf (parcel-order-log order)
          (append log
                  (list (list
                         (propertize (parcel-order-package order) 'face
                                     (pcase (parcel-order-status order)
                                       ('finished 'parcel-finished)
                                       ('blocked  'parcel-blocked)
                                       ('failed   'parcel-failed)
                                       (_         '(:weight bold))))
                         (time-subtract (current-time) parcel--order-queue-start-time)
                         text))))))

(defun parcel--events (&rest packages)
  "Return sorted event log string for PACKAGES.
If PACKAGES is nil, use all available orders."
  (let* ((orders
          (delete-dups
           (if packages
               (mapcar (lambda (package)
                         (alist-get (intern package)
                                    parcel--queued-orders))
                       packages)
             (mapcar #'cdr parcel--queued-orders))))
         (logs (apply #'append (mapcar #'parcel-order-log orders))))
    (mapconcat (lambda (event)
                 (pcase-let ((`(,package ,time ,text) event))
                   (format "[%s]%s %s"
                           (format-time-string "%02s.%3N" time)
                           (format "%-20s" (concat "("package"):"))
                           text)))
               (cl-sort (copy-tree logs) #'time-less-p :key #'cadr)
               "\n")))

(defun parcel-print-log (&rest packages)
  "Print log for PACKAGES."
  (interactive (completing-read-multiple "Log for Packages: "
                                         (mapcar #'car parcel--queued-orders)))
  (with-current-buffer (get-buffer-create "*Parcel Log*")
    (with-silent-modifications
      (erase-buffer)
      (insert (apply #'parcel--events packages))
      (display-buffer (current-buffer)))
    (special-mode)))

(defun parcel--update-order-status (order &optional status info)
  "Update ORDER STATUS.
Print the order status line in `parcel-status-buffer'.
If STATUS is non-nil and differs from ORDER's current STATUS,
signal ORDER's depedentents to check (and possibly change) their status.
If INFO is non-nil, ORDER's info is updated as well."
  (when-let (status
             (current-status (parcel-order-status order))
             ((not (eq current-status status))))
    (setf (parcel-order-status order) status)
    (cl-pushnew status (parcel-order-statuses order))
    (when (member status '(finished failed blocked))
      (mapc #'parcel--order-check-status (parcel-order-dependents order)))
    (when (eq status 'ref-checked-out)
      (mapc (lambda (o)
              (unless (member (parcel-order-statuses o) '(finished build-linked))
                (parcel--link-build-files o)
                (parcel--clone-dependencies o)))
            (parcel-order-includes order))))
  (when info
    (setf (parcel-order-info order) info)
    (parcel--log-event order info))
  (with-current-buffer (get-buffer-create parcel-status-buffer)
    (save-excursion
      (cursor-intangible-mode -1)
      (goto-char (point-min))
      (let* ((inhibit-read-only t)
             (index (parcel-order-index order))
             (found (and
                     (zerop (forward-line index))
                     (not (and (eobp) (= index (1- (length parcel--queued-orders))))))))
        (unless found (goto-char (point-max)))
        (unless (or (bobp) found) ; Don't want a newline before first process.
          (insert (propertize "\n" 'cursor-intangible t 'read-only t)))
        (delete-region (line-beginning-position) (line-end-position))
        (insert (parcel-status-buffer-line order)))
      (cursor-intangible-mode)
      (setq header-line-format '(:eval (parcel--header-line))))))

(defun parcel--add-remotes (recipe)
  "Given RECIPE, add repo remotes."
  (let ((default-directory (parcel-repo-dir recipe)))
    (cl-destructuring-bind
        ( &key remotes package
          ((:host recipe-host))
          ((:protocol recipe-protocol))
          ((:repo recipe-repo))
          &allow-other-keys
          &aux
          (order (alist-get (intern package) parcel--queued-orders)))
        recipe
      (parcel--update-order-status order 'adding-remotes "Adding Remotes")
      (pcase remotes
        ("origin" nil)
        ((and (pred stringp) remote)
         (parcel-process-call "git" "remote" "rename" "origin" remote))
        ((pred listp)
         (dolist (spec remotes)
           (if (stringp spec)
               (parcel--add-remotes (plist-put (copy-tree recipe) :remotes spec))
             (pcase-let ((`(,remote . ,props) spec))
               (if props
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
        (_ (parcel--update-order-status
            order 'failed
            (format "(wrong-type-argument ((stringp listp)) %S" remotes)))))))

(defun parcel--checkout-ref-process-filter (process output)
  "Filter PROCESS OUTPUT of async checkout-ref operation."
  (process-put process :result (concat (process-get process :result) output))
  (let ((order  (process-get process :order))
        (result (process-get process :result)))
    (parcel--update-order-status order
                                 (when (string-match-p "fatal" result) 'failed)
                                 (parcel-process-tail output))))

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

(defun parcel--files (order &optional files)
  "Return alist of ORDER :files to be symlinked: (PATH . TARGET PATH).
FILES is used recursively."
  (let* ((default-directory (parcel-order-repo-dir order))
         (build-dir         (parcel-order-build-dir order))
         (recipe            (parcel-order-recipe order))
         (files             (or files (plist-get recipe :files)))
         (exclusions        nil)
         (targets
          (cl-remove-if-not
           (lambda (path) (file-exists-p
                           (expand-file-name (if (stringp path) path (car path)))))
           (cl-set-difference
            (flatten-tree
             (mapcar (lambda (el)
                       (pcase el
                         ((pred stringp) (or (file-expand-wildcards el) (list el)))
                         (`(:exclude  . ,excluded)
                          (push (parcel--files order excluded) exclusions)
                          nil)
                         (:defaults
                          (parcel--files order parcel-default-files-directive))))
                     files))
            (flatten-tree exclusions)))))
    (mapcar (lambda (target)
              (cons (expand-file-name target)
                    (expand-file-name (file-name-nondirectory target) build-dir)))
            targets)))

(defun parcel--link-build-files (order)
  "Link ORDER's :files into it's builds subdirectory."
  (let* ((build-dir (parcel-order-build-dir order)))
    (parcel--update-order-status order 'linking-build "Linking build files")
    (when (file-exists-p build-dir) (delete-directory build-dir 'recusrive))
    (make-directory build-dir 'parents)
    (dolist (spec (parcel--files order))
      (let ((file   (car spec))
            (link   (cdr spec)))
        (make-directory (file-name-directory link) 'parents)
        (make-symbolic-link file link 'overwrite))))
  (parcel--update-order-status order 'build-linked "Build files linked"))

(defun parcel--clone-dependencies (order)
  "Clone ORDER's dependencies."
  (parcel--update-order-status order 'cloning-dependencies "Cloning Dependencies")
  (let* ((recipe       (parcel-order-recipe order))
         (build        (plist-get recipe :build))
         (dependencies (parcel--dependencies recipe))
         (emacs        (assoc 'emacs dependencies))
         (externals    (cl-remove-duplicates
                        (cl-remove-if (lambda (dependency)
                                        (member dependency parcel-ignored-dependencies))
                                      dependencies :key #'car)))
         (parcel--build-functions build))
    (if (and emacs (version< emacs-version (cadr emacs)))
        (parcel--update-order-status
         order 'failed (format "Requires %S; running %S" emacs emacs-version))
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
                      (append (parcel-order-dependencies order) (list dependency)))
                (push order (parcel-order-dependents dep-order))
                (if queued
                    (when (eq (parcel-order-status queued) 'finished) (cl-incf finished))
                  (if included
                      ;; Unblock dependency published in same repo...
                      (when blocked (parcel--clone-dependencies dep-order))
                    (unless blocked
                      (parcel-clone (parcel-order-recipe dep-order) 'force))))))
            (when (= (length externals) finished) ; Our dependencies beat us to the punch
              (parcel--update-order-status order 'buildling "Building...")
              (run-hook-with-args 'parcel--build-functions order)))
        (parcel--update-order-status order 'buildling "Building...")
        (run-hook-with-args 'parcel--build-functions order)))))

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
              (parcel--update-order-status
               order 'failed
               (format "Unable to check out ref: %S " (string-trim stderr))))))
        (unless (eq (parcel-order-status order) 'failed)
          (parcel--update-order-status order 'ref-checked-out "Ref checked out")
          (parcel--link-build-files order)
          (parcel--clone-dependencies order))))))

(defun parcel--checkout-ref (order)
  "Checkout ORDER's :ref.
The :branch and :tag keywords are syntatic sugar and are handled here, too."
  (parcel--update-order-status order 'checking-out-ref "Checking out repo ref")
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
    (let* ((process (make-process
                     :name     (format "parcel-fetch-%s" package)
                     :command  (list "git" "fetch" "--all")
                     :filter   #'parcel--checkout-ref-process-filter
                     :sentinel #'parcel--checkout-ref-process-sentinel)))
      (process-put process :order order)
      (setf (parcel-order-process order) process))))

(defvar parcel--queued-orders nil "List of queued orders.")

(defun parcel--emacs-path ()
  "Return path to running Emacs."
  (concat invocation-directory invocation-name))

(defun parcel-generate-autoloads (package dir)
  "Generate autoloads in DIR for PACKAGE."
  (let* ((auto-name (format "%s-autoloads.el" package))
         (output    (expand-file-name auto-name dir))
         (autoload-timestamps nil)
         (backup-inhibited t)
         (version-control 'never))
    (unless (file-exists-p output)
      (require 'autoload)
      (write-region (autoload-rubric output "package" nil) nil output nil 'silent))
    (make-directory-autoloads dir output)
    (when-let ((buf (find-buffer-visiting output)))
      (kill-buffer buf))
    auto-name))

(eval-and-compile
  (defun parcel--ensure-list (obj)
    "Ensure OBJ is a list."
    (if (listp obj) obj (list obj))))

(defmacro parcel-thread-callbacks (&rest fns)
  "Place each FN in FNS in callback position of previous FN."
  (let* ((reversed (reverse fns))
         (last `((lambda () ,(parcel--ensure-list (pop reversed))))))
    (mapc (lambda (fn)
            (setq last `((lambda () ,(append (parcel--ensure-list fn) last)))))
          reversed)
    ;; Ditch wrapping lambda of first call
    (nth 2 (pop last))))

(defun parcel--header-line ()
  "Return header line format for `parcel-buffer'."
  (let ((counts nil)
        (queue-len (length parcel--queued-orders)))
    (dolist (queued parcel--queued-orders)
      (let ((status (parcel-order-status (cdr queued))))
        (if (alist-get status counts)
            (cl-incf (alist-get status counts))
          (push (cons status 1) counts))))
    (concat
     (propertize " Parcel " 'face '(:weight bold))
     " "
     ;;@FIX: shouldn't have to use the quadruple %
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
             (or (alist-get 'failed   counts) 0)))))

(defun parcel--initialize-process-buffer ()
  "Initialize the parcel process buffer."
  (with-current-buffer (get-buffer-create parcel-status-buffer)
    (with-silent-modifications (erase-buffer))
    (unless (derived-mode-p 'parcel-status-mode)
      (parcel-status-mode))
    (setq header-line-format '(:eval (parcel--header-line)))
    (display-buffer (current-buffer))))

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
             'cursor-intangible t
             'front-sticky      t
             'package           package
             'order             order)
            " ")))

(defun parcel--queue-order (item &optional status)
  "Queue (ITEM . ORDER) in `parcel--queued-orders'.
If STATUS is non-nil, the order is given that initial status.
RETURNS order structure."
  (if (alist-get item parcel--queued-orders)
      (warn "%S already queued. Duplicate?" item)
    (let* ((status  (or status 'queued))
           (info    "Package queued")
           (package (if (listp item) (car item) item))
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
           (order
            (make-parcel-order
             :package (format "%S" package) :recipe recipe :status status
             :steps (plist-get recipe :build) :info info :repo-dir repo-dir
             :build-dir (when recipe (parcel-build-dir recipe))))
           (mono-repo
            (cl-some (lambda (cell)
                       (when-let ((queued (cdr cell))
                                  ((and repo-dir
                                        (equal repo-dir (parcel-order-repo-dir queued)))))
                         queued))
                     parcel--queued-orders)))
      (prog1 order
        (push (cons package order) parcel--queued-orders)
        (if (not mono-repo)
            (parcel--update-order-status order)
          (cl-pushnew order (parcel-order-includes mono-repo))
          (if (memq 'ref-checked-out (parcel-order-statuses mono-repo))
              (progn
                (parcel--link-build-files order)
                (parcel--clone-dependencies order))
            (parcel--update-order-status order 'blocked
                                         (format "Waiting for monorepo %S" repo-dir))))))))

(defun parcel--clone-process-filter (process output)
  "Filter PROCESS OUTPUT of async clone operation."
  (process-put process :result (concat (process-get process :result) output))
  (let ((order  (process-get process :order))
        (result (process-get process :result)))
    (parcel--update-order-status order (cond
                                        ((string-match-p "Username" result) 'blocked))
                                 (parcel-process-tail output))))

(defun parcel--clone-process-sentinel (process _event)
  "Sentinel for clone PROCESS."
  (let ((order  (process-get process :order))
        (result (process-get process :result)))
    (if (and (string-match-p "fatal" result)
             (not (string-match-p "already exists" result)))
        (parcel--update-order-status order 'failed)
      (let ((recipe (parcel-order-recipe order)))
        (parcel--add-remotes recipe)
        (parcel--checkout-ref order)))))

(defun parcel--order-check-status (order)
  "Called when one of an ORDER's dependencies have changed status.
Possibly kicks off next build step, or changes order status."
  (let* ((statuses
          (mapcar (lambda (dependency)
                    (let ((order (alist-get dependency parcel--queued-orders)))
                      (cons (parcel-order-package order)
                            (parcel-order-status  order))))
                  (parcel-order-dependencies order)))
         (blocked (cl-remove-if (lambda (status) (eq status 'finished))
                                statuses :key #'cdr))
         (failed  (cl-remove-if-not (lambda (status) (eq status 'failed))
                                    statuses :key #'cdr)))
    (cond
     (failed
      (parcel--update-order-status
       order 'failed (format "Failed dependencies: %S" (mapcar #'car failed))))
     (blocked
      (parcel--update-order-status
       order 'blocked (format "Blocked by dependencies: %S" (mapcar #'car blocked))))
     ((cl-every (lambda (status) (eq (cdr status) 'finished)) statuses)
      (let ((parcel--build-functions (plist-get (parcel-order-recipe order) :build)))
        (run-hook-with-args 'parcel--build-functions order))))))

(defun parcel-clone (recipe &optional force)
  "Clone repo to `parcel-directory' from RECIPE.
If FORCE is non-nil, ignore order queue."
  (cl-destructuring-bind
      ( &key package depth &allow-other-keys
        &aux
        (item              (intern package))
        (order             (alist-get item parcel--queued-orders))
        (repodir           (if order
                               (parcel-order-repo-dir order)
                             (parcel-repo-dir recipe)))
        (URI               (parcel--repo-uri recipe))
        (default-directory parcel-directory))
      recipe
    (when (or force (not order))
      (when (not order) (setq order (parcel--queue-order item)))
      (setf (parcel-order-status order) 'cloning)
      (let ((process (make-process
                      :name     (format "parcel-clone-%s" package)
                      :command  `("git" "clone"
                                  ;;@TODO: certain refs will necessitate full clone
                                  ;; or specific branch...
                                  ,@(when depth (list "--depth" (number-to-string depth)))
                                  ,URI ,repodir)
                      :filter   #'parcel--clone-process-filter
                      :sentinel #'parcel--clone-process-sentinel)))
        (process-put process :order order)
        (setf (parcel-order-process order) process)))))

(defun parcel--generate-autoloads-async-process-filter (process output)
  "Filter autoload PROCESS OUTPUT."
  (process-put process :result (concat (process-get process :result) output))
  (let ((order  (process-get process :order))
        (result (process-get process :result)))
    ;;@TODO: Warn on failure?
    ;;@FIX: debugger blocks and causes abnormal exit in sentinel
    (parcel--update-order-status order
                                 (when (string-match-p "Debugger entered" result) 'blocked)
                                 (parcel-process-tail output))))

(defun parcel--finish-order-maybe (order)
  "If ORDER has is finished, declare it finished.
Clean up process reference.
Retrun t if process has finished, nil otherwise."
  (unless (or (eq (parcel-order-status order) 'finished)
              (parcel-order-steps order))
    (parcel--update-order-status order 'finished "✓")
    ;; Remove stale reference so object can be deleted.
    (setf (parcel-order-process order) nil)
    t))

(defun parcel--generate-autoloads-async-process-sentinel (process event)
  "PROCESS autoload generation EVENT."
  (when (equal event "finished\n")
    (let ((order  (process-get process :order)))
      (setf (parcel-order-steps order) (cl-remove 'parcel--generate-autoloads-async
                                                  (parcel-order-steps order)))
      (unless (eq (parcel-order-status order) 'failed)
        (parcel--update-order-status order 'autoloads-generated "Autoloads Generated")
        (parcel--activate-package order)
        (parcel--finish-order-maybe order)))))

(defun parcel--generate-autoloads-async (order)
  "Generate ORDER's autoloads.
Async wrapper for `parcel-generate-autoloads'."
  (parcel--log-event order "Generating autoloads")
  (let* ((emacs     (parcel--emacs-path))
         (package   (parcel-order-package  order))
         (build-dir (parcel-order-build-dir order))
         (parcel    (expand-file-name "parcel/" parcel-directory))
         (command   (list emacs "-Q"
                          "-L" parcel
                          "-L" build-dir ; Is this necessary?
                          "-l" (expand-file-name "parcel.el" parcel)
                          "--batch" "--eval"
                          (format "(parcel-generate-autoloads %S %S)" package build-dir)))
         (process   (make-process
                     :name     (format "parcel-autoloads-%s" package)
                     :command  command
                     :filter   #'parcel--generate-autoloads-async-process-filter
                     :sentinel #'parcel--generate-autoloads-async-process-sentinel)))
    (process-put process :order order)))

(defun parcel--activate-package (order)
  "Activate ORDER's package."
  (parcel--update-order-status order 'activating "Activating package")
  (setf (parcel-order-steps order)
        (cl-remove 'parcel--activate-package (parcel-order-steps order)))
  (let* ((default-directory (parcel-order-build-dir order))
         (package           (parcel-order-package order))
         (autoloads         (format "%s-autoloads.el" package)))
    (add-to-list 'load-path default-directory)
    (parcel--update-order-status order nil "Package build dir added to load-path")
    (condition-case err
        (progn
          (load autoloads nil 'nomessage)
          (parcel--update-order-status order 'activated "Package activated"))
      ((error) (parcel--update-order-status order 'failed-to-activate
                                            (format "Failed to load %S: %S" autoloads err))))
    (parcel--finish-order-maybe order)))

(defun parcel--byte-compile-process-filter (process output)
  "Filter async byte-compilation PROCESS OUTPUT."
  (process-put process :result (concat (process-get process :result) output))
  (let ((order  (process-get process :order))
        (result (process-get process :result)))
    (parcel--update-order-status order
                                 (when (string-match-p "Debugger entered" result) 'failed)
                                 (parcel-process-tail output))))

(defun parcel--byte-compile-process-sentinel (process event)
  "PROCESS byte-compilation EVENT."
  (when (equal event "finished\n")
    (let ((order (process-get process :order)))
      (setf (parcel-order-steps order) (cl-remove 'parcel--byte-compile
                                                  (parcel-order-steps order)))
      (unless (eq (parcel-order-status order) 'failed)
        (parcel--update-order-status order 'byte-compiled "Successfully byte compiled")
        (parcel--finish-order-maybe order)))))

(defun parcel--byte-compile (order)
  "Byte compile package from ORDER."
  ;; Assumes all dependencies are 'built
  (parcel--update-order-status order 'byte-compiling "Byte compiling")
  (let* ((build-dir         (parcel-order-build-dir order))
         (default-directory build-dir)
         (emacs             (parcel--emacs-path))
         ;;@MAYBE: fix if we decide to store order objects in order :dependencies
         (dependency-dirs
          (mapcar (lambda (item) (parcel-order-build-dir
                                  (alist-get item parcel--queued-orders)))
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
           :name     (format "parcel-byte-compile-%s" (parcel-order-recipe order))
           :command  `(,emacs "-Q" "--batch" "--eval" ,(format "%S" program))
           :filter   #'parcel--byte-compile-process-filter
           :sentinel #'parcel--byte-compile-process-sentinel)))
    (process-put process :order order)))

;;;; COMMANDS
;;;###autoload
(defun parcel-delete-repos (&optional force)
  "Remove everything except parcel from `parcel-directory'.
If FORCE is non-nil, do not ask for confirmation."
  (interactive "P")
  (when (or force (yes-or-no-p "Remove all parcel repos?"))
    (mapc (lambda (file)
            (unless (member (file-name-nondirectory file) '("." ".." "parcel"))
              (delete-directory file 'recursive)))
          (directory-files parcel-directory 'full))))

;;;; STATUS BUFFER
(defvar parcel-status-mode-map (let ((map (make-sparse-keymap)))
                                 (define-key map (kbd "<return>") 'parcel-status-mode-send-input)
                                 (define-key map (kbd "C-c C-r")  'parcel-status-mode-visit-repo)
                                 (define-key map (kbd "C-c C-b")  'parcel-status-mode-visit-build)
                                 map))

(define-derived-mode parcel-status-mode text-mode "Parcel Status Mode"
  "Mode for interacting with the parcel status buffer.

\\{parcel-status-mode-map}"
  (cursor-intangible-mode))

(defun parcel-status-mode-send-input ()
  "Send input string to current process."
  (interactive)
  (if-let ((order (get-text-property (line-beginning-position) 'order))
           (process (parcel-order-process order)))
      (let ((input (save-excursion
                     (beginning-of-line)
                     (while (get-text-property (point) 'read-only)
                       (forward-char))
                     (string-trim (buffer-substring (point) (line-end-position))))))
        (process-send-string process (concat input "\n"))
        (end-of-line))
    (user-error "No process associated with current package")))

(defun parcel-status-mode-visit-repo ()
  "Visit repo associated with current process."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if-let ((order (get-text-property (point) 'order))
             (dir   (parcel-order-repo-dir order))
             ((file-exists-p dir)))
        (dired dir)
      (user-error "No repo dir associated with current line"))))

;;@TODO: consolidate with above
(defun parcel-status-mode-visit-build ()
  "Visit builds dir associated with current process."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if-let ((order (get-text-property (point) 'order))
             (dir   (parcel-order-build-dir order))
             ((file-exists-p dir)))
        (dired dir)
      (user-error "No build dir associated with current line"))))

(provide 'parcel)
;;; parcel.el ends here
