;;; elpaca-use-package.el --- Elpaca use-package support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Nicholas Vollmer

;; Author: Nicholas Vollmer
;; URL: https://github.com/progfolio/elpaca
;; Package-Requires: ((emacs "27.1") (elpaca "0") (use-package "2.4.4"))
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
(require 'use-package)

(defun elpaca-use-package--normalizer (_name keyword args)
  "Return ARGS passed to an Elpaca-handled use-package KEYWORD."
  (if (or args (eq keyword :vc)) (car (last args)) t))

(defun elpaca-use-package--handler (name _keyword ensure rest state)
  "Handle an Elpaca-managed use-package keyword.
NAME, ENSURE, REST, STATE as per `use-package' handler conventions.
ENSURE may be:
  - t or nil: install/skip as feature NAME
  - SYMBOL: install under that package name
  - (ID . PROPS...): Elpaca order with explicit ID
  - (PROPS...): plist recipe; NAME is prepended as the ID."
  (let ((processed (use-package-process-keywords name rest state)))
    (when (memq (car-safe ensure) '(quote \`)) (setq ensure (eval ensure t)))
    (if-let* ((order (cond
                      ((null ensure) nil)
                      ((or (eq ensure t) (eq (car-safe ensure) t)) name)
                      ((and (consp ensure) (keywordp (car ensure))) (cons name ensure))
                      (t ensure))))
        `((elpaca ,order ,@processed))
      processed)))

;;;###autoload
(define-minor-mode elpaca-use-package-mode
  "Minor mode to enable Elpaca support for `use-package'."
  :global t :group 'elpaca
  (if elpaca-use-package-mode
      (progn
        (advice-add #'use-package-handler/:ensure :override #'elpaca-use-package--handler)
        (advice-add #'use-package-normalize/:ensure :override #'elpaca-use-package--normalizer))
    (advice-remove #'use-package-handler/:ensure #'elpaca-use-package--handler)
    (advice-remove #'use-package-normalize/:ensure #'elpaca-use-package--normalizer)))

(defun elpaca-use-package--vc-to-order (name spec)
  "Convert a package-vc SPEC plist for package NAME into an Elpaca recipe.
Keys with no Elpaca equivalent are silently ignored."
  (let ((recipe (list :package (symbol-name name)
                      :repo (plist-get spec :url))))
    (when-let* ((branch (plist-get spec :branch)))
      (setq recipe (plist-put recipe :branch branch)))
    (when-let* ((main (plist-get spec :main-file)))
      (setq recipe (plist-put recipe :main main)))
    (when-let* ((lisp-dir (plist-get spec :lisp-dir)))
      (setq recipe (plist-put recipe :files
                              (list (concat (file-name-as-directory lisp-dir) "*.el")
                                    :defaults))))
    (cons name recipe)))

(defun use-package-handler/:vc (name _keyword spec rest state)
  "Convert and install the package-vc SPEC for NAME into an Elpaca."
  `((elpaca ,(elpaca-use-package--vc-to-order name spec)
            ,@(use-package-process-keywords name rest state))))

(defalias 'use-package-normalize/:vc #'elpaca-use-package--normalizer)
(defalias 'use-package-handler/:elpaca #'elpaca-use-package--handler)
(defalias 'use-package-handler/:straight #'elpaca-use-package--handler)
(defalias 'use-package-normalize/:elpaca #'elpaca-use-package--normalizer)
(defalias 'use-package-normalize/:straight #'elpaca-use-package--normalizer)
(add-to-list 'use-package-keywords :vc)
(add-to-list 'use-package-keywords :elpaca)
(add-to-list 'use-package-keywords :straight)

(provide 'elpaca-use-package)
;;; elpaca-use-package.el ends here
