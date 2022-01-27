;;; parcel-menu-melpa.el --- MELPA menu support for parcel  -*- lexical-binding: t; -*-

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

;;  MELPA support for parcel.

;;; Code:
(require 'cl-lib)

(defcustom parcel-menu-melpa-path
  (expand-file-name "melpa/" (temporary-file-directory))
  "Path where MELPA repository is cloned."
  :type 'directory
  :group 'parcel)

(defvar parcel-menu-melpa--index-cache nil "Cache of index.")

;;@TODO: needs to be more robust if processes error
(defun parcel-menu-melpa--clone (path)
  "Clone MELPA recipes repo to PATH."
  (make-directory path)
  (message "Downloading MELPA recipes...")
  (call-process "git" nil nil nil "init")
  (call-process "git" nil nil nil "config" "core.sparseCheckout" "true")
  (shell-command "echo \"recipes\" >> .git/info/sparse-checkout")
  (call-process "git" nil nil nil "remote" "add" "origin" "https://www.github.com/melpa/melpa.git")
  (call-process "git" nil nil nil "pull" "--depth=1")
  (call-process "git" nil nil nil "checkout" "master")
  (message "MELPA recipes downloaded."))

(defun parcel-menu-melpa--update ()
  "Update recipes in MELPA menu."
  (message "Checking Melpa for updates...")
  (condition-case _
      (progn
        (call-process "git" nil nil nil "pull")
        (message "MELPA updates downloaded"))
    ((error) (message "Unable to pull MELPA recipes"))))

(defun parcel-menu-melpa--index ()
  "Return candidate list of available MELPA recipes."
  (or parcel-menu-melpa--index-cache
      (setq parcel-menu-melpa--index-cache
            (mapcar (lambda (file)
                      (with-temp-buffer
                        (insert-file-contents file)
                        (condition-case-unless-debug _
                            (when-let ((recipe (read (buffer-string)))
                                       (package (pop recipe))
                                       ((member (plist-get recipe :fetcher) '(git github gitlab))))
                              (setq recipe
                                    (append (list :package (symbol-name package)) recipe))
                              (cons (intern-soft (file-name-nondirectory file))
                                    (list :source "MELPA" :recipe recipe)))
                          ((error) (message "parcel-menu-melpa couldn't process %S" file) nil))))
                    (directory-files "./recipes/" 'full "\\(?:^[^.]\\)")))))

;;;###autoload
(defun parcel-menu-melpa (request)
  "Delegate REQUEST.
If REQUEST is `index`, return a recipe candidate alist.
If REQUEST is `update`, update the MELPA recipe cache."
  (let* ((repo (file-name-as-directory parcel-menu-melpa-path))
         (default-directory repo))
    (unless (file-exists-p repo) (parcel-menu-melpa--clone repo))
    (pcase request
      ('index  (parcel-menu-melpa--index))
      ('update (setq parcel-menu-melpa--index-cache nil)
               (parcel-menu-melpa--update)))))

(provide 'parcel-menu-melpa)
;;; parcel-menu-melpa.el ends here
