;;; parcel-use-package.el --- use-package :parcel keyword support  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nicholas Vollmer

;; Author:  Nicholas Vollmer
;; Keywords:

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
;; see: https://github.com/jwiegley/use-package#keyword-extensions

;;

;;; Code:
(require 'cl-lib)
(require 'use-package-core)

;;add to use-package-keywords
(cl-pushnew :parcel use-package-keywords)

;;normalizer

(defalias 'use-package-normalize/:parcel #'parcel-use-package--normalizer)
(defun parcel-use-package--normalizer (name-symbol keyword args)
  "Normalize the :parcel `use-package' keyword.
NAME-SYMBOL, KEYWORD, and ARGS @TODO."
  (let ((parsed-args nil))
    (dolist (arg args (reverse parsed-args))
      (cond
       ((null arg) (setq parsed-args nil))
       ((eq arg t) (push name-symbol parsed-args))
       ((symbolp arg) (push arg parsed-args))
       ((not (listp arg))
        (use-package-error ":parcel wants a symbol or list"))
       ;; lists
       (t (let ((c (car arg)))
            (cond
             ((keywordp c)
              ;; recipe without package name
              (push (cons name-symbol arg) parsed-args))
             ((cl-some #'keywordp arg)
              ;; assume it's a recipe
              (push arg parsed-args))
             ;; recipe with only a package name
             ((and (symbolp c) (not (member c '(\` quote))))
              (push arg parsed-args))
             ;; normalize backquoted/quoted arg and preserve quote
             (t (push (list c (car (parcel-use-package--normalizer
                                    name-symbol keyword (cdr arg))))
                      parsed-args)))))))))

;;handler

(defalias 'use-package-handler/:parcel #'parcel-use-package--handler)
(defun parcel-use-package--handler (name-symbol _keyword archive-name rest state)
  "Handler for `:straight' in `use-package' forms.
NAME-SYMBOL, KEYWORD, ARCHIVE-NAME, REST, and STATE are explained by the
`use-package' documentation."
  (append
   (mapcar (lambda (arg)
             `(parcel
                  ,(if (member (car-safe arg) '(\` quote))
                       arg
                     (macroexpand `(quote ,(if (eq arg t) name-symbol arg))))
                (with-eval-after-load 'use-package
                  ,@(use-package-process-keywords name-symbol rest state))))
           archive-name)))

(defun parcel-use-package-unload-function ()
  "Clean up when parcel-use-package is unloaded."
  (when use-package-keywords
    (setq use-package-keywords (remq :parcel use-package-keywords))))

(provide 'parcel-use-package)
;;; parcel-use-package.el ends here
