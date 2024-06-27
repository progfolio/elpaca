;;; elpaca-use-package.el --- Elpaca use-package support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Nicholas Vollmer

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

(defun elpaca-use-package--handler (name keyword ensure rest state)
  "See `use-package-handler/:ensure'for NAME KEYWORD ENSURE REST STATE."
  (when (memq keyword '(:straight :elpaca)) ;;@COMPAT
    (lwarn '(elpaca-use-package-compat) :warning
           "%s: %s use-package declaration's %S keyword converted to :ensure"
           (or load-file-name (buffer-name)) name keyword)
    (plist-put rest :ensure nil))
  (let ((processed (use-package-process-keywords name rest state)))
    (cond ((null ensure) processed) ; :ensure nil = no Elpaca integration
          ;; :ensure t or `use-package-always-ensure' non-nil = (elpaca NAME ...)
          ((or (eq ensure t) (equal ensure '(t))) (setq ensure name))
          ;; Handle ID/NAME mismatch
          ((and (listp ensure) (keywordp (car ensure))) (push name ensure)))
    (if ensure `((elpaca ,ensure ,@processed)) processed)))

(defun elpaca-use-package--normalizer (_name _keyword args)
  "Return first arg of ARGS passed to use-package's :ensure function."
  (if args (car (last args)) t)) ; Bare :ensure implies t in vanilla use-package.

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

;;@COMPAT: Remove :elpaca/:straight keywords support eventually.
(define-obsolete-variable-alias 'elpaca-use-package-by-default 'use-package-always-ensure "2024-02-08")
(defalias 'use-package-handler/:straight #'use-package-handler/:ensure)
(defalias 'use-package-handler/:elpaca #'use-package-handler/:ensure)
(defalias 'use-package-normalize/:straight #'elpaca-use-package--normalizer)
(defalias 'use-package-normalize/:elpaca #'elpaca-use-package--normalizer)
(add-to-list 'use-package-keywords :straight)
(add-to-list 'use-package-keywords :elpaca)

(provide 'elpaca-use-package)
;;; elpaca-use-package.el ends here
