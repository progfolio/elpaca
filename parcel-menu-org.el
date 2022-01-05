;;; parcel-menu-org.el --- Parcel menu for Org packages  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  No Wayman

;; Author: No Wayman <iarchivedmywholelife@gmail.com>
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

;; A parcel menu for Org packages

;;; Code:
(require 'parcel)
(require 'parcel-process)

(defvar parcel-menu-org--index-cache nil "Cache of Org menu index.")

(defun parcel-menu-org--build ()
  "Generate `org-version.el`."
  (let* ((default-directory (expand-file-name "./org/lisp" parcel-directory))
         (orgversion
          (parcel-with-process
              (parcel-process-call "git" "describe" "--match" "release*" "--abbrev=0" "HEAD")
            (if failure
                ;; Backup in case where Org repo has no tags
                (parcel-with-process
                    (parcel-process-call
                     "emacs" "-Q" "--batch"
                     "--eval" "(require 'lisp-mnt)"
                     "--visit" "org.el"
                     "--eval" "(princ (lm-header \"version\"))")
                  (if failure
                      (error "Failed to parse ORGVERSION")
                    (replace-regexp-in-string "-dev" "" stdout)))
              (string-trim (replace-regexp-in-string "release_" "" stdout)))))
         (gitversion
          (concat orgversion "-g"
                  (string-trim (parcel-process-output "git" "rev-parse" "--short=6" "HEAD"))))
         (emacs (concat invocation-directory invocation-name)))
    (call-process
     emacs nil "*parcel-byte-compilation*" nil
     "-Q" "--batch"
     "--eval" "(setq vc-handled-backends nil org-startup-folded nil)"
     "--eval" "(add-to-list 'load-path \".\")"
     "--eval" "(load \"org-compat.el\")"
     "--eval" "(load \"../mk/org-fixup.el\")"
     "--eval" "(org-make-org-loaddefs)" ; Should we handle autoloads?
     "--eval" (format "(org-make-org-version %S %S)" orgversion gitversion)
     "--eval" "(cd \"../doc\")"
     "--eval" "(org-make-manuals)")))

;;;###autoload
(defun parcel-menu-org (request)
  "If REQUEST is `index`, return Org recipe candidate list."
  (when (eq request 'index)
    (or parcel-menu-org--index-cache
        (setq parcel-menu-org--index-cache
              (list
               (cons 'org
                     (list  :source "Org"
                            :recipe
                            (list
                             :package "org"
                             :type 'git
                             :repo "https://git.savannah.gnu.org/git/emacs/org-mode.git"
                             :depth 'full ; `org-version' depends on repository tags.
                             :pre-build '(parcel-menu-org--build)
                             :build '(:not autoloads)
                             :files '(:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*")))))
               (cons 'org-contrib
                     (list :source "Org"
                           :recipe
                           (list
                            :package "org-contrib"
                            :type 'git
                            :includes
                            '(ob-arduino ; Intentionally short for indentation
                              ob-clojure-literate ob-csharp ob-eukleides
                              ob-fomus ob-julia ob-mathematica ob-mathomatic ob-oz
                              ob-php ob-redis ob-sclang ob-smiles ob-spice ob-stata
                              ob-tcl ob-vbnet ol-bookmark ol-elisp-symbol ol-git-link
                              ol-man ol-mew ol-notmuch ol-vm ol-wl org-annotate-file
                              org-attach-embedded-images org-bibtex-extras
                              org-checklist org-choose org-collector org-contacts
                              org-contribdir org-depend org-effectiveness org-eldoc
                              org-eval org-eval-light org-expiry
                              org-interactive-query org-invoice org-learn org-license
                              org-mac-iCal org-mac-link org-mairix org-notify
                              org-panel org-passwords org-registry org-screen
                              org-screenshot org-secretary org-static-mathjax
                              org-sudoku orgtbl-sqlinsert org-toc org-track
                              org-velocity org-wikinodes ox-bibtex ox-confluence
                              ox-deck ox-extra ox-freemind ox-groff ox-koma-letter
                              ox-rss ox-s5 ox-taskjuggler)
                            :repo "https://git.sr.ht/~bzg/org-contrib"
                            :files '(:defaults "lisp/*.el")))))))))

(provide 'parcel-menu-org)
;;; parcel-menu-org.el ends here
