;;; parcel.el --- An elisp package manager           -*- lexical-binding: t; -*-

;; Copyright (C) 2022  No Wayman

;; Author: No Wayman <iarchivedmywholelife@gmail.com>
;; Keywords: tools, convenience, lisp

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
(require 'cl-lib)

(defgroup parcel nil
  "An elisp package manager."
  :group 'parcel
  :prefix "parcel-")
(defun parcel-merge-plists (&rest plists)
  "Merge PLISTS. Keys on later lists override keys on earlier lists."
  (cl-reduce (lambda (parent child)
               (dolist (key (cl-remove-if-not #'keywordp child) parent)
                 (setq parent (plist-put parent key (plist-get child key)))))
             plists))

(provide 'parcel)
;;; parcel.el ends here
