;;; elpaca-tests.el --- test suite for elpaca.el      -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025 Nicholas Vollmer

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
(require 'elpaca)

;;prevent user cache corruption
(setq elpaca-cache-directory
      (expand-file-name "elpaca-tests-cache/" (temporary-file-directory)))
(setq elpaca-cache-autoloads nil)

(defun elpaca-tests--plist-equal-p (&rest plists)
  "Return t if PLISTS have same keys and values (order independent)."
  (let* ((basis (car plists))
         (keys  (cl-remove-if-not #'keywordp basis)))
    (cl-every (lambda (plist)
                (cl-every (lambda (key) (equal (plist-get basis key)
                                               (plist-get plist key)))
                          keys))
              (cdr plists))))

(defun elpaca-tests-menu (request)
  "A test menu. REQUEST RECIPE."
  (let ((recipes '((:package "a" :host gitlab :repo "a/a")
                   ( :package "elpaca" :host github :repo "progfolio/elpaca"
                     :pre-build ("./pre-build")))))
    (pcase request
      ('index  (mapcar (lambda (recipe) (cons
                                         (intern-soft (plist-get recipe :package))
                                         (list :source "test-menu"
                                               :recipe recipe)))
                       recipes))
      (_ (user-error "Unimplmented request method: %S" request)))))

(ert-deftest elpaca-merge-plists ()
  "Merges PLISTS."
  (let ((foo '(:foo 1))
        (bar '(:bar nil))
        (baz '(:baz 3))
        (nobaz '(:baz nil)))
    (should-not (elpaca-merge-plists nil))
    (should (elpaca-tests--plist-equal-p '(:foo 1 :bar nil :baz 3)
                                         (elpaca-merge-plists foo bar baz)))
    (should (elpaca-tests--plist-equal-p '(:foo 1 :bar nil :baz nil)
                                         (elpaca-merge-plists
                                          (elpaca-merge-plists foo bar baz)
                                          nobaz)))
    (should (elpaca-tests--plist-equal-p '(:foo 1) (elpaca-merge-plists '(:foo 1))))))

(ert-deftest elpaca-menu-item ()
  "Return menu item from MENUS."
  (should
   (let ((elpaca-menu-functions '(elpaca-tests-menu))
         elpaca--menu-cache)
     (elpaca-tests--plist-equal-p
      '(elpaca :source "test-menu"
               :recipe (:package "elpaca"
                                 :host github
                                 :repo "progfolio/elpaca"
                                 :pre-build
                                 ("./pre-build")))
      (elpaca-menu-item 'elpaca))))
  (should-not (let ((elpaca-menu-functions '(ignore))
                    elpaca--menu-cache)
                (elpaca-menu-item 'elpaca))))

(defun elpaca-tests--no-inheritance (order)
  "Turn off ORDER inheritance."
  '(:inherit nil))

(ert-deftest elpaca-recipe ()
  "Compute recipe from ORDER."
  (let ((elpaca-recipe-functions nil))
    ;; no inheritance, full spec
    (should (elpaca-tests--plist-equal-p
             (elpaca-recipe '(elpaca :host github :repo "progfolio/elpaca" :inherit nil))
             '(:package "elpaca" :host github :repo "progfolio/elpaca" :inherit nil)))
    ;; basic default inheritance
    (let ((elpaca-order-functions '(elpaca-tests--no-inheritance)))
      (should (elpaca-tests--plist-equal-p
               (elpaca-recipe '(elpaca :host github :repo "progfolio/elpaca"))
               '(:package "elpaca" :host github :repo "progfolio/elpaca" :inherit nil))))
    ;; basic menu lookup
    (let ((elpaca-order-functions '(elpaca-tests--no-inheritance))
          (elpaca-menu-functions '(elpaca-tests-menu))
          elpaca--menu-cache)
      (should (elpaca-tests--plist-equal-p
               '( :package "elpaca"
                  :host github
                  :repo "progfolio/elpaca"
                  :inherit nil)
               (elpaca-recipe '(elpaca :repo "progfolio/elpaca" :host github))))
      (should (elpaca-tests--plist-equal-p
               '( :package "elpaca"
                  :host github
                  :repo "progfolio/elpaca"
                  :inherit t
                  :pre-build ("./pre-build"))
               (elpaca-recipe '(elpaca :inherit t)))))))

(provide 'elpaca-tests)
;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; elpaca-tests.el ends here
