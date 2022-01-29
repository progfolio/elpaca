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

(defcustom parcel-directory (expand-file-name "parcel" user-emacs-directory)
  "Location of the parcel package store."
  :type 'directory)

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

(defcustom parcel-recipe-functions nil
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

(defvar parcel-ignored-dependencies '(cl-lib org map)
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
  (sort (apply #'append
               (cl-loop for fn in parcel-menu-functions
                        for index = (funcall fn 'index)
                        when index collect index))
        (lambda (a b) (string-lessp (car a) (car b)))))

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
(defun parcel-recipe (&optional order)
  "Return recipe computed from ORDER.
ORDER is any of the following values:
  - nil. The order is prompted for.
  - a symbol which will be looked up via `parcel-menu-functions'
  - an order list."
  (interactive)
  (let ((parcel-overriding-prompt "Recipe: ")
        (interactive (called-interactively-p 'interactive))
        package
        ingredients)
    (cond
     ((or (null order) (symbolp order))
      (let ((menu-item (parcel-menu-item nil order)))
        (unless menu-item (user-error "No menu-item for %S" order))
        (push (run-hook-with-args-until-success 'parcel-order-functions order)
              ingredients)
        (push menu-item ingredients)))
     ((listp order)
      (setq package (pop order))
      (unless (parcel--inheritance-disabled-p order)
        (let ((mods (run-hook-with-args-until-success 'parcel-order-functions order)))
          (push mods ingredients)
          (when (or (plist-get order :inherit) (plist-get mods :inherit))
            (push (parcel-menu-item nil package) ingredients))))
      (setq ingredients (append ingredients (list order))))
     (t (signal 'wrong-type-argument `((null symbolp listp) . ,order))))
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
  (cl-destructuring-bind (&key local-repo repo fetcher (host fetcher) &allow-other-keys)
      recipe
    (expand-file-name
     (if (parcel--full-repo-protocol-p repo)
         (let ((url (url-generic-parse-url repo)))
           (require 'url-parse)
           (string-join
            (list
             (or local-repo
                 (file-name-sans-extension
                  (replace-regexp-in-string
                   ".*/" ""
                   (url-filename
                    (url-generic-parse-url (plist-get (parcel-recipe 'org) :repo))))))
             "_"
             (url-host url))
            "."))
       ;;repo-or-local-repo.user.host
       (string-join (list (or local-repo (parcel--repo-name repo))
                          (parcel--repo-user repo)
                          (symbol-name host))
                    "."))
     parcel-directory)))

(defun parcel--repo-uri (recipe)
  "Return repo URI from RECIPE."
  (cl-destructuring-bind (&key (protocol 'https)
                               fetcher
                               (host fetcher)
                               repo &allow-other-keys)
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
                                                `((github gitlab stringp) ,host))))))
        (format "%s%s%s%s.git" (car protocol) host (cdr protocol) repo)))))

(defun parcel--add-remotes (recipe)
  "Given RECIPE, add repo remotes."
  (let ((default-directory (parcel-repo-dir recipe)))
    (cl-destructuring-bind
        (&key remotes
              ((:host recipe-host))
              ((:protocol recipe-protocol))
              ((:repo recipe-repo)) &allow-other-keys)
        recipe
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
                      "git" "remote" "add" remote (parcel--repo-uri recipe)))
                 (unless (equal remote "origin")
                   (parcel-process-call "git" "remote" "rename" "origin" remote)))))))
        (_ (signal 'wrong-type-argument `((stringp listp) ,remotes ,recipe)))))))

(defun parcel--checkout-ref (recipe)
  "Checkout RECIPE's :ref.
The :branch and :tag keywords are syntatic sugar and are handled here, too."
  (let ((default-directory (parcel-repo-dir recipe)))
    (cl-destructuring-bind (&key ref branch tag remotes &allow-other-keys)
        recipe
      (when (or ref branch tag)
        (cond
         ((and ref branch) (warn "Recipe :ref overriding :branch %S" recipe))
         ((and ref tag)    (warn "Recipe :ref overriding :tag %S" recipe))
         ((and tag branch) (error "Recipe ambiguous :tag and :branch %S" recipe)))
        (unless remotes    (signal 'wrong-type-argument
                                   `((stringp listp) ,remotes ,recipe)))
        (parcel-process-call "git" "fetch" "--all")
        (let* ((remote (if (stringp remotes) remotes (caar remotes))))
          (parcel-with-process
              (apply #'parcel-process-call
                     `("git"
                       ,@(delq nil
                               (cond
                                (ref    (list "checkout" ref))
                                (tag    (list "checkout" (concat ".git/refs/tags/" tag)))
                                (branch (list "switch" "-C" branch
                                              (format "%s/%s" remote branch)))))))
            (if success t
              (error "Unable to check out ref: %S %S" stderr recipe))))))))

(defun parcel--initialize-repo (recipe)
  "Using RECIPE, Clone repo, add remotes, check out :ref."
  (parcel-clone recipe)
  (parcel--add-remotes recipe)
  (parcel--checkout-ref recipe))

(defun parcel--dependencies (recipe)
  "Using RECIPE, compute package's dependencies.
If package's repo is not on disk, error."
  (let* ((default-directory (parcel-repo-dir recipe))
         (pkg (expand-file-name (format "%s-pkg.el" (plist-get recipe :package))))
         (defined (file-exists-p pkg))
         (main (format "%s.el" (plist-get recipe :package))))
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

(cl-defstruct parcel-order
  "Order object for queued processing."
  (package      nil :type string)
  (recipe       nil :type list)
  (status       nil :type string)
  (dependencies nil :type list)
  (dependents   nil :type list)
  (info         nil))

(defun parcel--queue-order (item &optional status)
  "Queue (ITEM . ORDER) in `parcel--queued-orders'.
If STATUS is non-nil, the order is given that initial status.
RETURNS order structure."
  (let* ((status  (or status 'queued))
         (info    "Package queued")
         (package (if (listp item) (car item) item))
         (recipe  (condition-case err
                      (parcel-recipe item)
                    ((error)
                     (setq status 'failed
                           info (format "No recipe: %S" err)))))
         (order (make-parcel-order
                 :package (format "%S" package) :recipe recipe :status status :info info)))
    (prog1 order
      (push (cons package order) parcel--queued-orders)
      (parcel--update-order-status package status info))))

(defun parcel--initialize-process-buffer ()
  "Initialize the parcel process buffer."
  (with-current-buffer (get-buffer-create parcel-status-buffer)
    (with-silent-modifications (erase-buffer))
    (unless (derived-mode-p 'parcel-status-mode)
      (parcel-status-mode))
    (display-buffer (current-buffer))))

(defun parcel-status-buffer-line (package status output &rest props)
  "Return STATUS string for PACKAGE with OUTPUT.
The package name is propertized with PROPS."
  (let ((name (format "%-15s"
                      (propertize package 'face
                                  (pcase status
                                    ('blocked '(:foreground "pink" :weight bold))
                                    ('failed  '(:foreground "red"  :weight bold))
                                    (_        '(:weight bold)))))))
    (concat (apply #'propertize
                   `(,(format "%s %s" name (or output ""))
                     read-only t
                     cursor-intangible t
                     front-sticky t
                     package ,package
                     ,@props))
            " ")))

(defun parcel--update-order-status (package status line &rest props)
  "Replace or append PACKAGE STATUS LINE to `parcel-status-buffer'.
PROPS are added to package string.
PACKAGE STATUS is also updated in `parcel--queued-orders'."
  (unless (stringp package) (setq package (format "%S" package))) ;ensure string
  (with-current-buffer (get-buffer-create parcel-status-buffer)
    (goto-char (point-min))
    (if-let ((anchor (text-property-search-forward 'package package t)))
        (goto-char (prop-match-end anchor))
      (goto-char (point-max))
      (unless (bobp) ; Don't want a newline before first process.
        (insert (propertize "\n" 'cursor-intangible t 'read-only t))))
    (with-silent-modifications
      (delete-region (line-beginning-position) (line-end-position))
      (insert (apply #'parcel-status-buffer-line
                     `(,package ,status ,line ,@props))))))

(defun parcel--clone-process-filter (process output)
  "Filter PROCESS OUTPUT of async clone operation."
  (process-put process :result (concat (process-get process :result) output))
  (with-current-buffer parcel-status-buffer
    (let ((result  (process-get process :result)))
      (parcel--update-order-status
       (parcel-order-package (process-get process :order))
       (when (string-match-p "Username" result)
         ;;@TODO: update dependents status to blocked
         'blocked)
       (parcel-process-tail output)
       'process process))))

(defun parcel-clone (recipe &optional force)
  "Clone repo to `parcel-directory' from RECIPE.
If FORCE is non-nil, ignore order queue."
  (cl-destructuring-bind
      ( &key package depth &allow-other-keys
        &aux
        (item              (intern package))
        (order             (alist-get item parcel--queued-orders))
        (repodir           (parcel-repo-dir recipe))
        (URI               (parcel--repo-uri recipe))
        (default-directory parcel-directory))
      recipe
    (when (or force (not order))
      (when (not order) (setq order (parcel--queue-order item)))
      (setf (parcel-order-status order) 'cloning)
      (let ((process (make-process
                      :name    (format "parcel-clone-%s" package)
                      :command `("git" "clone"
                                 ;;@TODO: certain refs will necessitate full clone
                                 ;; or specific branch...
                                 ,@(when depth (list "--depth" (number-to-string depth)))
                                 ,URI ,repodir)
                      :filter #'parcel--clone-process-filter
                      :sentinel (lambda (_proc event)
                                  (cond
                                   ((equal event "finished\n")))))))
        (process-put process :order order)))))

(defun parcel-clone-deps-aysnc (recipe &optional _callback)
  "Clone RECIPE's dependencies, then CALLBACK."
  (dolist (spec (parcel--dependencies recipe))
    (pcase-let ((`(,dependency ,version) spec))
      (if (equal dependency 'emacs)
          (when (version< emacs-version version)
            (error "Emacs version too low for %S: %S"
                   (plist-get recipe :package)
                   recipe))
        (unless (member dependency parcel-ignored-dependencies)
          (let ((recipe (parcel-recipe dependency)))
            (parcel-clone recipe)))))))

;; (defun parcel--process-dependencies (recipe)
;;   "Using RECIPE, compute dependencies and kick off their subprocesses."
;;   (dolist (dependency (parcel--dependencies recipe))
;;     (pcase-let ((`(,package ,version) dependency))
;;       (if (equal package 'emacs)
;;           (when (version< emacs-version version)
;;             (error "Emacs version too low for %S: %S"
;;                    (plist-get recipe :package)
;;                    recipe))
;;         (unless (member package parcel-ignored-dependencies)
;;           (parcel package #'parcel--process-dependencies))))))

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
(define-derived-mode parcel-status-mode text-mode "Parcel Status Mode"
  "Mode for interacting with the parcel status buffer."
  (cursor-intangible-mode))

(defun parcel-status-mode-send-input ()
  "Send input string to current process."
  (interactive)
  (when-let ((process (get-text-property (line-beginning-position) 'process)))
    (unless (eq (process-status process) 'run)
      (user-error "Process is no longer running: %S" (process-name process)))
    (let ((input (save-excursion
                   (beginning-of-line)
                   (while (get-text-property (point) 'read-only)
                     (forward-char))
                   (string-trim (buffer-substring (point) (line-end-position))))))
      (process-send-string process (concat input "\n"))
      (end-of-line))))

(defun parcel-status-mode-visit-repo ()
  "Visit repo associated with current process."
  (interactive)
  (save-excursion)
  (beginning-of-line)
  (if-let ((process  (get-text-property (point) 'process))
           (recipe   (parcel-order-recipe (process-get process :order)))
           (dir      (parcel-repo-dir recipe))
           ((file-exists-p dir)))
      (dired (parcel-repo-dir recipe))
    (user-error "No repo dir associated with current line")))

(defvar parcel-status-mode-map (let ((map (make-sparse-keymap)))
                                 (define-key map (kbd "<return>")   'parcel-status-mode-send-input)
                                 (define-key map (kbd "S-<return>") 'parcel-status-mode-visit-repo)
                                 map))

(provide 'parcel)
;;; parcel.el ends here
