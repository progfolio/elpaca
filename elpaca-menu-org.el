;;; elpaca-menu-org.el --- Elpaca menu for Org packages  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025 Nicholas Vollmer

;; Author: Nicholas Vollmer
;; Keywords: convenience

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

;; An Elpaca menu for Org packages

;;; Code:
(require 'elpaca)
(defvar elpaca-menu-org--index-cache nil "Cache of Org menu index.")
(defcustom elpaca-menu-org-make-manual t "When non-nil build Org manual."
  :type 'boolean :group 'elpaca)

(defun elpaca-menu-org--build ()
  "Generate `org-version.el`.
`default-directory' is assumed to be org's repo dir."
  (let* ((default-directory (expand-file-name "lisp/"))
         (emacs (elpaca--emacs-path))
         (orgversion (elpaca-process-cond ( emacs "-Q" "--batch"
                                            "--eval" "(require 'lisp-mnt)"
                                            "--visit" "org.el"
                                            "--eval" "(princ (lm-header \"version\"))")
                       (failure (error "Failed to parse ORGVERSION: %S" result))
                       (t (string-trim (replace-regexp-in-string "-dev" "" stdout)))))
         (gitversion (elpaca-process-cond ("git" "rev-parse" "--short=6" "HEAD")
                       (success (concat orgversion "-n/a-g" stdout))
                       (t (message "%S" stderr) "N/A"))))
    (message "Org version: %s %s" orgversion gitversion)
    (defvar org-startup-folded)
    (defvar org-element-cache-persistent)
    (setq vc-handled-backends nil org-startup-folded nil org-element-cache-persistent nil)
    (add-to-list 'load-path default-directory)
    (load "./org-compat.el")
    (load "../mk/org-fixup.el")
    (when (fboundp 'org-make-org-loaddefs)
      (message "Making loaddefs")
      (org-make-org-loaddefs))
    (when (fboundp 'org-make-org-version)
      (message "Making org-version")
      (org-make-org-version orgversion gitversion))
    (when (and elpaca-menu-org-make-manual (fboundp 'org-make-manual))
      (cd "../doc")
      (message "Making manual")
      (org-make-manual))))

;;;###autoload
(defun elpaca-menu-org (request &optional item)
  "Return menu ITEM REQUEST."
  (if-let* (((eq request 'index))
            (cache (or elpaca-menu-org--index-cache (elpaca-menu-org 'update))))
      (if item (elpaca-alist-get item cache) cache)
    (setq elpaca-menu-org--index-cache
          (list
           (cons 'org
                 (list :source "Org"
                       :description "Outline-based notes management and organizer"
                       :url "https:/orgmode.org"
                       :recipe
                       (list
                        :package "org"
                        :pre-build `(progn (require 'elpaca-menu-org)
                                           (setq elpaca-menu-org-make-manual ,elpaca-menu-org-make-manual)
                                           (elpaca-menu-org--build))
                        :host 'github :repo "emacsmirror/org"
                        :autoloads "org-loaddefs.el" :depth 1
                        :build '(:not elpaca--generate-autoloads-async)
                        :files '(:defaults ("etc/styles/" "etc/styles/*" "doc/*.texi")))))
           (cons 'org-contrib
                 (list :source "Org"
                       :description "Contributed Org packages in search of new maintainers"
                       :url "https://github.com/emacsmirror/org-contrib"
                       :recipe
                       (list
                        :package "org-contrib"
                        :host 'github :repo "emacsmirror/org-contrib"
                        :files '(:defaults))))))))

(provide 'elpaca-menu-org)
;;; elpaca-menu-org.el ends here
