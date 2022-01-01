;;; parcel-test.el --- test suite for parcel.el      -*- lexical-binding: t; -*-

;; Copyright (C) 2022  No Wayman

;; Author: No Wayman <iarchivedmywholelife@gmail.com>
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
(require 'ert)
(require 'parcel)

(defun parcel-test--plist-equal-p (&rest plists)
  "Return t if PLISTS have same keys and values (order independent)."
  (let* ((basis (car plists))
         (keys  (cl-remove-if-not #'keywordp basis)))
    (cl-every (lambda (plist)
                (cl-every (lambda (key) (equal (plist-get basis key)
                                               (plist-get plist key)))
                          keys))
              (cdr plists))))

(ert-deftest parcel-merge-plists ()
  "Merges PLISTS."
  (let ((foo '(:foo 1))
        (bar '(:bar nil))
        (baz '(:baz 3))
        (nobaz '(:baz nil)))
    (should-not (parcel-merge-plists nil))
    (should (parcel-test--plist-equal-p '(:foo 1 :bar nil :baz 3)
                                        (parcel-merge-plists foo bar baz)))
    (should (parcel-test--plist-equal-p '(:foo 1 :bar nil :baz nil)
                                        (parcel-merge-plists
                                         (parcel-merge-plists foo bar baz)
                                         nobaz)))
    (should (parcel-test--plist-equal-p '(:foo 1) (parcel-merge-plists '(:foo 1))))))



(provide 'parcel-test)
;;; parcel-test.el ends here
