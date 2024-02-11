;;; elpaca-menu.el --- Elpaca recipes                -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Nicholas Vollmer

;; Author: No Wayman <iarchivedmywholelife@gmail.com>
;; Keywords: convenience, lisp

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

;;

;;; Code:
(require 'elpaca)

(defun elpaca-menu--org-build ()
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
    (when (fboundp 'org-make-manual)
      (cd "../doc")
      (message "Making manual")
      (org-make-manual))))

(defvar elpaca-menu--index
  '((auctex . ( :description "Integrated TeX environment"
                :url "https://elpa.gnu.org/packages/auctex.html"
                :recipe ( :pre-build (("./autogen.sh")
                                      ("./configure"
                                       "--without-texmf-dir"
                                       "--with-packagelispdir=./"
                                       "--with-packagedatadir=./")
                                      ("make"))
                          :build (:not elpaca--compile-info) ;; Make will take care of this step
                          :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
                          :version (lambda (_) (require 'tex-site) AUCTeX-version))))
    (elpaca-use-package . ( :description "Elpaca use-package support."
                            :url "https://github.com/progfolio/elpaca"
                            :recipe ( :package "elpaca-use-package"
                                      :repo "https://github.com/progfolio/elpaca.git"
                                      :files '("extensions/elpaca-use-package.el")
                                      :main "extensions/elpaca-use-package.el"
                                      :build (:not elpaca--compile-info))))
    (key-chord . ( :description  "map pairs of simultaneously pressed keys to commands"
                   :url "https://github.com/emacsorphanage/key-chord"
                   :recipe ( :host github :repo "emacsorphanage/key-chord"
                             :depth nil ;; git tags for versioning
                             :version-regexp "\\(?:[[:digit:]]+\\.[[:digit:]]+\\)")))
    (org . ( :description "Outline-based notes management and organizer"
             :url "https:/orgmode.org"
             :recipe ( :package "org"
                       :local-repo "org"
                       :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git"
                       :pre-build (progn (require 'elpaca-menu) (elpaca-menu-org--build))
                       :autoloads "org-loaddefs.el"
                       :build (:not elpaca--generate-autoloads-async)
                       :files (:defaults ("etc/styles/" "etc/styles/*" "doc/*.texi")))))
    (org-contrib . ( :description "Contributed Org packages in search of new maintainers"
                     :url "https://git.sr.ht/~bzg/org-contrib"
                     :recipe ( :package "org-contrib"
                               :local-repo "org-contrib"
                               :repo "https://git.sr.ht/~bzg/org-contrib"
                               :files (:defaults))))))

;;;###autoload
(defun elpaca-menu (request)
  "Return recipes for `index` REQUEST."
  (and (eq request 'index) elpaca-menu--index))

(provide 'elpaca-menu)
;;; elpaca-menu.el ends here
