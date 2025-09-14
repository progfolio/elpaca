;;; elpaca-file.el --- Elpaca support for installing from elisp files -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author:  Nicholas Vollmer
;; Keywords: files

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

(cl-defmethod elpaca-source-dir ((e (elpaca file)))
  "Return source directory for :type `file` E."
  (file-name-directory (elpaca--main-file e)))

;;@MAYBE: remote file support? Security implications...
(cl-defmethod elpaca-source ((e (elpaca file)))
  "Populate source directory for :type `file` E."
  (elpaca--signal e (format "Skipping local file %S" (elpaca<-main e)) 'getting)
  (elpaca--continue-build e))

(cl-defmethod elpaca--delete ((e (elpaca file)))
  "Delete :type `file` E. Does not delete source directory."
  (let ((copy (copy-tree e))) ;;@HACK: replace source with build dir.
    (setf (elpaca<-source-dir copy) (elpaca<-build-dir copy))
    (cl-call-next-method copy)))

(provide 'elpaca-file)
;;; elpaca-file.el ends here
