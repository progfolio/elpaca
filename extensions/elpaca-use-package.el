;;; elpaca-use-package.el --- Elpaca use-package support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023  Nicholas Vollmer

;; Author: Nicholas Vollmer
;; URL: https://github.com/progfolio/elpaca
;; Package-Requires: ((emacs "27.1") (use-package "2.4.4"))
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
;; use-package support for Elpaca

;;; Code:
(require 'cl-lib)
(require 'use-package)

(defvar elpaca-use-package--original-keywords use-package-keywords)
(defvar elpaca-use-package-ignored-keywords '(:pin :ensure :load-path :requires))
(defcustom elpaca-use-package-by-default nil
  "When non-nil, automatically add :elpaca t to `use-package' declarations."
  :type 'boolean :group 'elpaca)

;; Respects `:if', `:when', `:unless'
(defun use-package-normalize/:elpaca (name _keyword args)
  "Return `use-package' declaration with NAME's KEYWORD ARGS."
  (let ((arg (car args)))
    (if (and (listp arg) (keywordp (car arg))) (list (cons name arg)) args)))

(defun use-package-handler/:elpaca (name _keyword args rest state)
  "Expand `use-package' declaration with NAME's body.
KEYWORD is :elpaca, and ARGS are the declaration values associated with it.
REST is the plist of args declared after the :elpaca keyword.
see `use-package' docs for STATE."
  ;; This happens at macro expansion time
  (let* ((rest (cl-loop for i below (length rest)
                        for el = (nth i rest)
                        if (and (keywordp el) (memq el elpaca-use-package-ignored-keywords))
                        do (cl-incf i)
                        else collect el))
         (body (use-package-process-keywords name rest state))
         (arg (car args)))
    `((elpaca ,@(if (eq t arg) (list name) args) ,@body))))

(defun elpaca-use-package--maybe (fn &rest args)
  "Temporarily disable `elpaca-use-package-mode' for FN with ARGS if :elpaca nil."
  (let* ((pargs (cdr-safe args))
         (declared (member :elpaca pargs)))
    (if (or (cadr declared) (and (not declared) elpaca-use-package-by-default))
        (apply fn args)
      (setq args (cl-loop for i below (length args)
                          for arg = (nth i args)
                          if (eq arg :elpaca) do (cl-incf i)
                          else collect arg))
      (elpaca-use-package-mode -1)
      (unwind-protect
          (apply fn args)
        (elpaca-use-package-mode 1)))))

(defun elpaca-use-package--setup ()
  "Setup Elpaca `use-package' support."
  (unless (memq :elpaca use-package-keywords)
    (setq use-package-keywords
          (let ((tail (memq :no-require use-package-keywords)))
            (append
             (cl-subseq use-package-keywords 0
                        (- (length use-package-keywords) (length tail)))
             (list :elpaca)
             tail)))
    ;;@HACK: re-ordering keywords is naughty according to use-package's docs.
    (setq use-package-keywords
          (append
           (cl-remove-if (lambda (kw)
                           (memq kw elpaca-use-package-ignored-keywords))
                         use-package-keywords)
           elpaca-use-package-ignored-keywords))
    (add-to-list 'use-package-defaults
                 (list :elpaca '(list t)
                       (lambda (&rest _) elpaca-use-package-by-default)))
    (advice-add #'use-package :around #'elpaca-use-package--maybe)))

(defun elpaca-use-package--teardown ()
  "Remove Elpaca `use-package' support."
  ;; Install keywords which were added while `elpaca-use-package-mode' was active.
  (cl-loop with reversed = (reverse use-package-keywords)
           for added in (cl-set-difference use-package-keywords
                                           (cons :elpaca elpaca-use-package--original-keywords))
           for target = (cadr (member added reversed))
           for position = (1+ (or (cl-position target elpaca-use-package--original-keywords) -1))
           do (push added (nthcdr position elpaca-use-package--original-keywords)))
  (setf use-package-keywords elpaca-use-package--original-keywords
        (alist-get :elpaca use-package-defaults nil 'remove) nil)
  (advice-remove #'use-package #'elpaca-use-package--maybe))

;;;###autoload
(define-minor-mode elpaca-use-package-mode
  "Minor mode to enable :elpaca support for `use-package'."
  :global t :group 'elpaca
  (if elpaca-use-package-mode (elpaca-use-package--setup) (elpaca-use-package--teardown)))

(provide 'elpaca-use-package)
;;; elpaca-use-package.el ends here
