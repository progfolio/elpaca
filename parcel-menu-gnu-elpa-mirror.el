;;; parcel-menu-gnu-elpa-mirror.el --- GNU ELPA menu support for parcel  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nicholas Vollmer

;; Author: Nicholas Vollmer
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

;;  GNU ELPA support for parcel.

;;; Code:
(require 'cl-lib)
(require 'parcel-process)

(defcustom parcel-menu-gnu-elpa-mirror-path
  (expand-file-name "gnu-elpa-mirror/" (temporary-file-directory))
  "Path where GNU ELPA repository is cloned."
  :type 'directory
  :group 'parcel)

(defvar parcel-menu-gnu-elpa-mirror--index-cache nil "Cache of index.")
(defvar parcel-menu-gnu-elpa-mirror-address
  "https://www.github.com/emacs-straight/gnu-elpa-mirror.git"
  "Address of the menu repository.")

(defun parcel-menu-gnu-elpa-mirror--clone ()
  "Clone GNU ELPA recipes repo to PATH."
  (message "Downloading GNU ELPA recipes...")
  (let ((default-directory user-emacs-directory))
    (parcel-with-process
        (parcel-process-call "git" "clone"
                             parcel-menu-gnu-elpa-mirror-address
                             parcel-menu-gnu-elpa-mirror-path)
      ;;@TODO: make stderr match more robust
      (if (or success (and stderr (string-match-p "already" stderr)))
          (message "GNU ELPA recipes downloaded.")
        (warn "Unable to download GNU ELPA recipes. This menu will not work!")))))

(defun parcel-menu-gnu-elpa-mirror--update ()
  "Update recipes in GNU ELPA menu."
  (message "Checking GNU ELPA for updates...")
  (condition-case _
      (progn
        (call-process "git" nil nil nil "pull")
        (message "GNU ELPA updates downloaded"))
    ((error) (message "Unable to pull GNU ELPA recipes"))))

(defun parcel-menu-gnu-elpa-mirror--index ()
  "Return candidate list of available GNU ELPA recipes."
  (or parcel-menu-gnu-elpa-mirror--index-cache
      (setq parcel-menu-gnu-elpa-mirror--index-cache
            (mapcar (lambda (file)
                      (when (file-exists-p (expand-file-name file default-directory))
                        (cons (intern file)
                              (list :source "GNU ELPA Mirror"
                                    :recipe (list :package file
                                                  :host 'github
                                                  :repo (concat "emacs-straight/" file))))))
                    (directory-files default-directory nil "\\(?:^[^.]\\)")))))

;;;###autoload
(defun parcel-menu-gnu-elpa-mirror (request)
  "Delegate REQUEST.
If REQUEST is `index`, return a recipe candidate alist.
If REQUEST is `update`, update the GNU ELPA recipe cache."
  (let* ((repo (file-name-as-directory parcel-menu-gnu-elpa-mirror-path))
         (default-directory repo))
    (unless (file-exists-p repo) (parcel-menu-gnu-elpa-mirror--clone))
    (pcase request
      ('index  (parcel-menu-gnu-elpa-mirror--index))
      ('update (setq parcel-menu-gnu-elpa-mirror--index-cache nil)
               (parcel-menu-gnu-elpa-mirror--update)))))

(provide 'parcel-menu-gnu-elpa-mirror)
;;; parcel-menu-gnu-elpa-mirror.el ends here
