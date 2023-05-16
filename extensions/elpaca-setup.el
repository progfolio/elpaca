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

(defun elpaca-setup--shorthand (sexp)
  "Retrieve feature from SEXP of :elpaca macro."
  (cadr sexp))

(defun elpaca-setup--find-orders (lst)
  "Find :elpaca setup macro in LST and return ELPACA ORDER."
  (let (orders)
    (while lst
      (if (and (consp (car lst))
	       (eq (caar lst) :elpaca))
	  (push (cdar lst) orders))
      (setq lst (cdr lst)))
    orders))

(defun elpaca-setup--extract-feat (name)
  "Returns the feature from `NAME' of `setup' function."
  (if (consp name)
      (let ((shorthand (get (car name) 'setup-shorthand)))
	(and shorthand (funcall shorthand name)))
    name))

(defmacro elpaca-setup--default-dependent-order-condition (use-elpaca-by-default name)
  "Returns the predicate for extracting list of orders from NAME.
If USE-PACKAGE-BY-DEFAULT is t, then target feature in NAME of
`setup' will be used as ORDER to `elpaca' by appropriate
shorthand of NAME."
  (if use-elpaca-by-default
      `(or (and (consp ,name)
		(or (and (eq :elpaca (car ,name)) (list (cdr ,name)))
		    (elpaca-setup--find-orders ,name)))
	   (list (elpaca-setup--extract-feat ,name)))
    `(and (consp ,name)
	  (or (and (eq :elpaca (car ,name)) (list (cdr ,name)))
	      (elpaca-setup--find-orders ,name)))))

;;;###autoload
(defmacro elpaca-setup-integrate (use-elpaca-by-default)
  "Add `elpaca' support to `setup'.
If USE-PACKAGE-BY-DEFAULT is t, then target feature in NAME of
`setup' will be used as ORDER to `elpaca' by appropriate
shorthand of NAME.

If `:elpaca' exists in both NAME and BODY of `setup', then the
one in BODY will be selected which helps you to override `elpaca'
ORDER if USE-ELPACA-BY-DEFAULT is t.

If there are multiple `:elpaca' exists in BODY of `setup', first
one will be chosen. If required so, use `elpaca' directly, or
open a new `setup' inside.

Note that definition of `setup' will be replaced but previous
definition will be in use under different name. So you should
call `elpaca-setup-integrate' after user customizations to
definition of `setup', such as advises."
  `(progn
     (fset 'elpaca-setup--setup-initial-definition (symbol-function #'setup))
     (put 'elpaca-setup--setup-initial-definition 'lisp-indent-function 1)
     
     (setup-define :elpaca
       (lambda (&rest _) t)
       :documentation "A placeholder SETUP macro that evaluates to t.
This will help when chaining `:elpaca' with other `setup' constructs, such as `:and'."
       :shorthand #'elpaca-setup--shorthand)

     (defmacro setup (name &rest body)
       (declare (indent 1))
       (if-let* ((orders (or (elpaca-setup--find-orders body)
			     (elpaca-setup--default-dependent-order-condition ,use-elpaca-by-default name)))
		 (body `(elpaca ,(car orders) ; place an order
				(elpaca-setup--setup-initial-definition ,(elpaca-setup--extract-feat name) ; now use setup again for expanding body, but don't re-evaluate name again
									,@body))))
	   (progn (dolist (order (cdr orders))
		    (setq body `(elpaca ,order ,body)))
		  `(elpaca-setup--setup-initial-definition ,name ; setup may be shortcircuited
							   ,body))
	 `(elpaca-setup--setup-initial-definition ,name ,@body))) ; no :elpaca, normal setup

     (put #'setup 'function-documentation (advice--make-docstring 'elpaca-setup--setup-initial-definition))))

;;;###autoload
(defun elpaca-setup-teardown ()
  "Remove `elpaca' support from `setup'.
Note that `setup' definition will be restored to the one when
`elpaca-setup-integrate' is called."
  (fset 'setup #'elpaca-setup--setup-initial-definition)
  (setq setup-macros (assoc-delete-all :elpaca setup-macros)))

