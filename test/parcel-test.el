;;; parcel-test.el --- test suite for parcel.el      -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Nicholas Vollmer

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

;;

;;; Code:
(require 'ert)
(require 'parcel)

;;prevent user cache corruption
(setq parcel-cache-menu-items nil)
(setq parcel-cache-orders nil)
(setq parcel-cache-autoloads nil)

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
  (let ((parcel-menu-functions '(parcel-test-menu))
        parcel-menu--candidates-cache)
    (should (parcel-test--plist-equal-p '( :package "parcel" :repo "progfolio/parcel"
                                           :host github
                                           :pre-build ("./pre-build"))
                                        (parcel-menu-item nil 'parcel)))
    (setq parcel-menu--candidates-cache nil)
    (should-not (parcel-menu-item nil 'parcel '(ignore)))))

(ert-deftest parcel-recipe ()
  "Compute recipe from ORDER."
  (let ((parcel-recipe-functions nil))
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
          (parcel-menu-functions '(parcel-test-menu))
          parcel-menu--candidates-cache)
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
               (parcel-recipe '(parcel :inherit t)))))))

(provide 'parcel-test)
;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; parcel-test.el ends here
