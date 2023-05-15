;;; elpaca-setup.el --- Elpaca setup.el support -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Sami Batuhan Basmaz Ölmez

;; Author: Sami Batuhan Basmaz Ölmez
;; URL: https://github.com/progfolio/elpaca
;; Package-Requires: ((emacs "27.1") (elpaca "0") (setup "1.3.2"))
;; Version: 0.0.0

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
;; setup.el support for Elpaca

(require 'setup)

(defvar elpaca-setup-feature-changer-constructs '((:with-feature . (:get-feat #'cadr :get-body #'cddr)))
  "Alist of keywords that change feature context of `setup'.
Each element of alist must be in the form of (KEYWORD . PLIST)
where PLIST must provide properties for `:get-feat' and
`:get-body' both with a value of a function which when called
with a sexp whose car is KEYWORD, returns the feature and body of
the sexp respectively.")

(defun elpaca-setup--shorthand (sexp)
  "Retrieve feature from SEXP of :elpaca macro."
  (let ((order (cadr sexp)))
    (if (consp order)
        (car order)
      order)))

(defun elpaca-setup--find-order (lst)
  "Find :elpaca setup macro in LST and return ELPACA ORDER."
  (let (order)
    (while (and lst (not order))
      (if (and (consp (car lst))
	       (eq (caar lst) :elpaca))
	  (setq order (cdar lst)))
      (setq lst (cdr lst)))
    order))

(defun elpaca-setup--extract-feat (name)
  "Returns the feature from `NAME' of `setup' function."
  (if (consp name)
      (let ((shorthand (get (car name) 'setup-shorthand)))
	(and shorthand (funcall shorthand name)))
    name))

(defun elpaca-setup--expand-feature-changer-constructs (body)
  "Expand recursive :elpaca calls where encapsulating feature is possibly different.
It is only meaningful to use `:elpaca' once for each different
features. This is also because there is one function body for
each different feature. This doesn't mean that you can't nest
`elpaca' for continuing feature. You can still make use of
`elpaca' function.

Note that you can also nest `setup' constructs."
  (let (expanded-body)
    (dolist (e body)
      (push (if-let ((getters (alist-get (car-safe e) elpaca-setup-feature-changer-constructs))
		     (order (elpaca-setup--find-order (funcall (plist-get getters :get-body) e))))
		`(elpaca ,order
			 (elpaca-setup--setup-initial-definition ,(funcall (plist-get getters :get-feat) e)
								 ,@(elpaca-setup--expand-feature-changer-constructs (cddr e))))
	      e)
	    expanded-body))
    (nreverse expanded-body)))

(defmacro elpaca-setup--default-dependent-order-condition (use-elpaca-by-default name)
  "Returns the predicate for extracting order from NAME.
If USE-PACKAGE-BY-DEFAULT is t, then target feature in NAME of
`setup' will be used as ORDER to `elpaca' by appropriate
shorthand of NAME."
  (if use-elpaca-by-default
      `(or (and (consp ,name)
		(or (and (eq :elpaca (car ,name)) (cdr ,name))
		    (elpaca-setup--find-order ,name)))
	   (elpaca-setup--extract-feat ,name))
    `(and (consp ,name)
	  (or (and (eq :elpaca (car ,name)) (cdr ,name))
	      (elpaca-setup--find-order ,name)))))

;;;###autoload
(defmacro elpaca-setup-integrate (use-elpaca-by-default)
  "Add `elpaca' support to `setup'.
If USE-PACKAGE-BY-DEFAULT is t, then target feature in NAME of
`setup' will be used as ORDER to `elpaca' by appropriate
shorthand of NAME.

If `:elpaca' exists in both NAME and BODY of `setup', then the
one in BODY will be selected which helps you to override `elpaca'
ORDER if USE-ELPACA-BY-DEFAULT is t.

If there are multiple `:elpaca' exists at same depth of BODY of
`setup', first one will be chosen. See
`elpaca-setup--expand-feature-changer-constructs' for details.

Note that definition of `setup' will be replaced but previous
definition will be in use under different name. So you should
call `elpaca-setup-integrate' after user customizations to
definition of `setup', such as advises."
  `(progn
     (fset 'elpaca-setup--setup-initial-definition (or setup-definition (symbol-function #'setup)))
     (put 'elpaca-setup--setup-initial-definition 'lisp-indent-function 1)
     
     (setup-define :elpaca
       (lambda (&rest _) t)
       :documentation "A placeholder SETUP macro that evaluates to t.
This will help when chaining `:elpaca' with other `setup' constructs, such as `:and'."
       :shorthand #'elpaca-setup--shorthand)

     (defmacro setup (name &rest body)
       (declare (indent 1))
       (if-let ((order (or (elpaca-setup--find-order body)
			   (elpaca-setup--default-dependent-order-condition ,use-elpaca-by-default name))))
	   `(elpaca-setup--setup-initial-definition ,name
						    (elpaca ,order
							    (elpaca-setup--setup-initial-definition ,(if (consp order)
													 (car order)
												       order)
												    ,@(elpaca-setup--expand-feature-changer-constructs body))))
	 `(elpaca-setup--setup-initial-definition ,name ,@(elpaca-setup--expand-feature-changer-constructs body))))

     (put #'setup 'function-documentation (advice--make-docstring 'elpaca-setup--setup-initial-definition))))

;;;###autoload
(defun elpaca-setup-teardown ()
  "Remove `elpaca' support from `setup'.
Note that `setup' definition will be restored to the one when
`elpaca-setup-integrate' is called."
  (fset 'setup #'elpaca-setup--setup-initial-definition)
  (setq setup-macros (assoc-delete-all :elpaca setup-macros)))

