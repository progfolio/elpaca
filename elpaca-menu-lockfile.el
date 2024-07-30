;;; elpaca-menu-lockfile.el --- lockfile menu        -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Nicholas Vollmer

;; Author:  Nicholas Vollmer
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

;;

;;; Code:
(require 'elpaca)

(defvar elpaca-menu-lockfile--index-cache
  (when (and elpaca-lock-file (file-exists-p elpaca-lock-file)) (elpaca--read-file elpaca-lock-file))
  "Cache of lockfile index.")

;;;###autoload
(defun elpaca-menu-lockfile (_)
  "Return locked package menu items."
  (or elpaca-menu-lockfile--index-cache
      (when (and elpaca-lock-file (file-exists-p elpaca-lock-file)) (elpaca--read-file elpaca-lock-file))))

(provide 'elpaca-menu-lockfile)
;;; elpaca-menu-lockfile.el ends here
