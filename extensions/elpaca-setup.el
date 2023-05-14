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

(defvar elpaca-setup-feature-changer-constructs '(:with-feature))

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

(defun elpaca-setup--call-shorthand (name)
  (if (consp name)
      (let ((shorthand (get (car name) 'setup-shorthand)))
	(and shorthand (funcall shorthand name)))
    name))

(defun elpaca-setup--expand-feature-changer-constructs (body)
  (let (expanded-body)
    (dolist (e body)
      (push (if-let (((memq (car-safe e) elpaca-setup-feature-changer-constructs))
		     (order (elpaca-setup--find-order (cddr e))))
		`(elpaca ,order
			 (elpaca-setup--setup-initial-definition ,(cadr e)
								 ,@(elpaca-setup--expand-feature-changer-constructs (cddr e))))
	      e)
	    expanded-body))
    (nreverse expanded-body)))

(defmacro elpaca-setup--default-dependent-order-condition (use-elpaca-by-default name)
  (if use-elpaca-by-default
      `(or (and (consp ,name)
		(or (and (eq :elpaca (car ,name)) (cdr ,name))
		    (elpaca-setup--find-order ,name)))
	   (elpaca-setup--call-shorthand ,name))
    `(and (consp ,name)
	  (or (and (eq :elpaca (car ,name)) (cdr ,name))
	      (elpaca-setup--find-order ,name)))))

;;;###autoload
(defmacro elpaca-setup-integrate (use-elpaca-by-default)
  `(progn
     (fset 'elpaca-setup--setup-initial-definition (symbol-function #'setup))
     (put 'elpaca-setup--setup-initial-definition 'lisp-indent-function 1)
     
     (setup-define :elpaca
       (lambda (&rest _) t)
       :documentation "A placeholder SETUP macro that evaluates to t."
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
  (fset 'setup #'elpaca-setup--setup-initial-definition)
  (setq setup-macros (assoc-delete-all :elpaca setup-macros)))

