;;; elpaca-dired.el --- dired support for Elpaca package store  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026  Nicholas Vollmer

;; Author: Nicholas Vollmer
;; URL: https://github.com/progfolio/elpaca
;; Created: Jun 20, 2026
;; Keywords: tools, convenience, files
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.0.0

;; This file is not part of GNU Emacs.

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

;;; Code:
(require 'elpaca)
(require 'dired)

(defun elpaca-dired--refresh ()
  "Add package name overlays after `dired-mode' package store directories."
  (remove-overlays (point-min) (point-max) 'elpaca-dired-overlay t)
  (when-let* ((rows (cl-loop for (_ . e) in (reverse (elpaca--queued)) collect
                             (cons (directory-file-name (elpaca<-source-dir e)) (elpaca<-package e)))))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (when-let* ((file (dired-get-filename nil t))
                    ((file-directory-p file))
                    (dir (directory-file-name file))
                    (prefix (file-name-as-directory dir))
                    (names (cl-loop  for (source . pkg) in rows
                                     for sourcedir = (file-name-as-directory source)
                                     when (or (equal source dir)
                                              (string-prefix-p prefix sourcedir))
                                     collect pkg))
                    ((dired-move-to-filename))
                    (ov (make-overlay (line-beginning-position) (line-end-position))))
          (overlay-put ov 'elpaca-dired-overlay t)
          (overlay-put ov 'after-string (propertize (format "  %s" (string-join names ", "))
                                                    'face 'font-lock-doc-face)))
        (forward-line 1)))))

;;;###autoload
(define-minor-mode elpaca-dired-mode
  "Display package names over hash-keyed directories in Dired."
  :lighter " Elpaca-Store"
  (if elpaca-dired-mode
      (progn (require 'dired)
             (add-hook 'dired-after-readin-hook #'elpaca-dired--refresh nil t)
             (elpaca-dired--refresh))
    (remove-hook 'dired-after-readin-hook #'elpaca-dired--refresh t)
    (remove-overlays (point-min) (point-max) 'elpaca-dired-overlay t)))

(provide 'elpaca-dired)
;;; elpaca-dired.el ends here
