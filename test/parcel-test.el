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

(defun parcel-test-menu (request)
  "A test menu. REQUEST RECIPE."
  (let ((recipes '((:package "a" :host gitlab :repo "a/a")
                   ( :package "parcel" :host github :repo "progfolio/parcel"
                     :pre-build ("./pre-build")))))
    (pcase request
      ('index  (mapcar (lambda (recipe) (cons
                                         (intern-soft (plist-get recipe :package))
                                         (list :source "test-menu"
                                               :recipe recipe)))
                       recipes))
      (_ (user-error "Unimplmented request method: %S" request)))))

(ert-deftest parcel-plist-p ()
  "Return t if passed a (:key val...) plist."
  (should (parcel-plist-p '(:key val)))
  (should (parcel-plist-p '(:key val :another val)))
  (should-not (parcel-plist-p nil))
  (should-not (parcel-plist-p 1))
  (should-not (parcel-plist-p '(:key)))
  (should-not (parcel-plist-p '(:key val val))))

(ert-deftest parcel-clean-plist ()
  "Remove unrecognized keywords."
  (should (parcel-test--plist-equal-p '(:host github)
                                      (parcel-clean-plist '(:fetcher t :host github)))))

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

(ert-deftest parcel-menu-item ()
  "Return menu item from MENUS."
  (let ((parcel-menu-functions '(parcel-test-menu)))
    (should (parcel-test--plist-equal-p '( :package "parcel" :repo "progfolio/parcel"
                                           :host github
                                           :pre-build ("./pre-build"))
                                        (parcel-menu-item nil 'parcel)))
    (should-not (parcel-menu-item nil 'parcel '(ignore)))))

(ert-deftest parcel-recipe ()
  "Compute recipe from ORDER."
  ;; no inheritance, full spec
  (should (parcel-test--plist-equal-p
           (parcel-recipe '(parcel :host github :repo "progfolio/parcel" :inherit nil))
           '(:package "parcel" :host github :repo "progfolio/parcel" :inherit nil)))
  ;; basic default inheritance
  (let ((parcel-order-functions '((lambda (order) '(:inherit nil)))))
    (should (parcel-test--plist-equal-p
             (parcel-recipe '(parcel :host github :repo "progfolio/parcel"))
             '(:package "parcel" :host github :repo "progfolio/parcel" :inherit nil))))
  ;; basic menu lookup
  (let ((parcel-order-functions '((lambda (order) '(:inherit nil))))
        (parcel-menu-functions '(parcel-test-menu)))
    (should (parcel-test--plist-equal-p
             '( :package "parcel"
                :host github
                :repo "progfolio/parcel"
                :inherit nil)
             (parcel-recipe '(parcel :repo "progfolio/parcel" :host github))))
    (should (parcel-test--plist-equal-p
             '( :package "parcel"
                :host github
                :repo "progfolio/parcel"
                :inherit t
                :pre-build ("./pre-build"))
             (parcel-recipe '(parcel :inherit t))))))

(provide 'parcel-test)
;;; parcel-test.el ends here
