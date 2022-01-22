;;; parcel.el --- An elisp package manager           -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nicholas Vollmer

;; Author: Nicholas Vollmer
;; URL: https://github.com/progfolio/parcel
;; Created: Jan 1, 2022
;; Keywords: tools, convenience, lisp
;; Package-Requires: ((emacs "26.1"))
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

(defgroup parcel nil
  "An elisp package manager."
  :group 'parcel
  :prefix "parcel-")

(defcustom parcel-directory (expand-file-name "parcel" user-emacs-directory)
  "Location of the parcel package store."
  :type 'directory)

(defcustom parcel-order-functions nil
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

(defcustom parcel-menu-functions '(parcel-menu-org parcel-menu-melpa)
  "Abnormal hook to lookup packages in menus.
Each function is passed a request, which may be any of the follwoing symbols:
  - `index`
     Must return a alist of the menu's package candidates.
     Each candidate is an cell of form:
     (PACKAGE-NAME . (:source SOURCE-NAME :recipe RECIPE-PLIST))
  - `update`
     Updates the menu's package candidate list."
  :type 'hook)

(defvar parcel-recipe-keywords (list :pre-build :branch :depth :fork :host
                                     :nonrecursive :package :protocol :remote :repo)
  "Recognized parcel recipe keywords.")

(defun parcel-plist-p (obj)
  "Return t if OBJ is a plist of form (:key val...)."
  (and obj
       (listp obj)
       (zerop (mod (length obj) 2))
       (cl-every #'keywordp (cl-loop for (key _) on obj by #'cddr collect key))))

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

(defvar parcel-overriding-prompt nil "Overriding prompt for interactive functions.")

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

(defun parcel--inheritance-disabled-p (plist)
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

(defun parcel--repo-name (string)
  "Return repo name portion of STRING."
  (substring string (1+ (string-match-p "/" string))))

(defun parcel--repo-user (string)
  "Return user name portion of STRING."
  (substring string 0 (string-match-p "/" string)))

;;@FIX: not robust
(defun parcel-repo-protocol-specified-p (string)
  "Return t if repo STRING has the full protocol embedded in it."
  (string-match-p ":" string))

(defun parcel-repo-dir (recipe)
  "Return path to repo given RECIPE."
  (cl-destructuring-bind (&key local-repo repo &allow-other-keys) recipe
    (when repo
      (expand-file-name
       (or local-repo
           (let* ((short (parcel--repo-name repo))
                  (user  (parcel--repo-user repo))
                  (possible (expand-file-name short parcel-directory)))
             ;;check remote to see if it belongs to us
             (if (file-exists-p possible)
                 (let* ((default-directory possible)
                        (remotes (parcel-process-output "git" "remote" "-v")))
                   (if (string-match-p (parcel--repo-uri recipe) remotes)
                       short
                     (format "%s-%s" short user)))
               short)))
       parcel-directory))))

(defun parcel--repo-uri (recipe)
  "Return repo URI from RECIPE."
  (cl-destructuring-bind (&key (protocol 'https)
                               fetcher
                               (host fetcher)
                               repo &allow-other-keys)
      recipe
    (let ((protocol (pcase protocol
                      ('https "https://")
                      ('ssh   "git@")
                      (_      (signal 'wrong-type-error `((http ssh) ,protocol)))))
          (host     (pcase host
                      ('github       "github.com")
                      ('gitlab       "gitlab.com")
                      ((pred stringp) host)
                      (_              (signal 'wrong-type-argument
                                              `((github gitlab stringp) ,host))))))
      (format "%s%s/%s.git" protocol host repo))))

(defalias 'parcel--add-remotes #'ignore)
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
        (let* ((_remote (if (stringp remotes) remotes (caar remotes))))
          (apply #'parcel-process-call
                 `("git"
                   ,@(delq nil
                           (cond
                            (ref    (list "checkout" ref))
                            (tag    (list "checkout" ".git/refs/tags" tag))
                            (branch (list "switch" "-C" branch)))))))))))

(defun parcel-clone (recipe)
  "Clone package repository to `parcel-directory' using RECIPE."
  (cl-destructuring-bind (&key fetcher (host fetcher) &allow-other-keys)
      recipe
    (unless host (user-error "No :host in recipe %S" recipe))
    (let* ((default-directory parcel-directory))
      ;;@TODO: handle errors
      (apply #'parcel-process-call
             (delq nil (list "git" "clone" (parcel--repo-uri recipe)
                             (parcel-repo-dir recipe)))))))

(defun parcel--initialize-repo (recipe)
  "Using RECIPE, Clone repo, add remotes, check out :ref."
  (thread-first recipe
                (parcel-clone)
                (parcel--checkout-ref)))

;;;###autoload
(defun parcel (order)
  "ORDER."
  (parcel--initialize-repo (parcel-recipe order)))

(declare-function autoload-rubric "autoload")
(defvar autoload-timestamps)
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

(provide 'parcel)
;;; parcel.el ends here
