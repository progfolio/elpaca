;;; parcel.el --- An elisp package manager           -*- lexical-binding: t; -*-

;; Copyright (C) 2022  No Wayman

;; Author: No Wayman <iarchivedmywholelife@gmail.com>
;; Keywords: tools, convenience, lisp

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

(defgroup parcel nil
  "An elisp package manager."
  :group 'parcel
  :prefix "parcel-")

(defcustom parcel-directory (expand-file-name "parcel" user-emacs-directory)
  "Location of the parcel package store."
  :type 'directory)

(defcustom parcel-order-functions nil
  "Abnormal hook run to alter orders.
Each element must be a unary function which accepts an order plist.
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
  (let (key value current plist)
    (while (setq current (pop plists))
      (while current
        (setq key   (pop current)
              value (pop current)
              plist (plist-put plist key value))))
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
     (t (signal 'wrong-type-error `((null symbolp listp) . ,order))))
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

(provide 'parcel)
;;; parcel.el ends here
