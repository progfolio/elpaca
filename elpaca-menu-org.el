;;; elpaca-menu-org.el --- Elpaca menu for Org packages  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nicholas Vollmer

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

;; A elpaca menu for Org packages

;;; Code:
(require 'elpaca)
(defvar elpaca-menu-org--index-cache nil "Cache of Org menu index.")

(defun elpaca-menu-org--build ()
  "Generate `org-version.el`.
`default-directory' is assumed to be org's repo dir."
  (let* ((default-directory (expand-file-name "lisp/"))
         (emacs (elpaca--emacs-path))
         (orgversion
          (elpaca-with-process
              (elpaca-process-call
               emacs "-Q" "--batch"
               "--eval" "(require 'lisp-mnt)"
               "--visit" "org.el"
               "--eval" "(princ (lm-header \"version\"))")
            (when failure (error "Failed to parse ORGVERSION: %S" result))
            (string-trim (replace-regexp-in-string "-dev" "" stdout))))
         (gitversion
          (elpaca-with-process (elpaca-process-call "git" "ls-remote" "--tags")
            (if-let  ((success)
                      (tag (car (last (split-string stdout "[[:space:]]+" 'omit-nulls))))
                      (ref (elpaca-process-output "git" "rev-parse" "--short=6" "HEAD")))
                (replace-regexp-in-string "\\(?:refs/tags/\\([^z-a]*\\)\\^{}\\)"
                                          (concat "\\1-n/a-g" (string-trim ref))
                                          tag)
              (message "%S" stderr)
              "N/A"))))
    (message "Org version: %s %s" orgversion gitversion)
    (defvar org-startup-folded)
    (setq vc-handled-backends nil org-startup-folded nil)
    (add-to-list 'load-path ".")
    (load "org-compat.el")
    (load "../mk/org-fixup.el")
    (when (fboundp 'org-make-org-loaddefs)
      (message "Making loaddefs")
      (org-make-org-loaddefs))
    (when (fboundp 'org-make-org-version)
      (message "Making org-version")
      (org-make-org-version orgversion gitversion))
    (when (fboundp 'org-make-manuals)
      (cd "../doc")
      (message "Making manuals")
      (org-make-manuals))))

;;;###autoload
(defun elpaca-menu-org (request)
  "If REQUEST is `index`, return Org recipe candidate list."
  (or (and (not (eq request 'update)) elpaca-menu-org--index-cache)
      (setq elpaca-menu-org--index-cache
            (list
             (cons 'org
                   (list :source "Org"
                         :description "Outline-based notes management and organizer"
                         :url "https:/orgmode.org"
                         :recipe
                         (list
                          :package "org"
                          :local-repo "org"
                          :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git"
                          :depth nil ; `org-version' depends on repository tags.
                          :pre-build '(progn (require 'elpaca-menu-org)
                                             (elpaca-menu-org--build))
                          :build '(:not elpaca--generate-autoloads-async
                                        elpaca--compile-info)
                          :files '(:defaults ("etc/styles/" "etc/styles/*")))))
             (cons 'org-contrib
                   (list :source "Org"
                         :description "Contributed Org packages in search of new maintainers"
                         :url "https://git.sr.ht/~bzg/org-contrib"
                         :recipe
                         (list
                          :package "org-contrib"
                          :local-repo "org-contrib"
                          :repo "https://git.sr.ht/~bzg/org-contrib"
                          :files '(:defaults))))))))

(provide 'elpaca-menu-org)
;;; elpaca-menu-org.el ends here
