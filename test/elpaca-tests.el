;;; elpaca-tests.el --- test suite for elpaca.el      -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Nicholas Vollmer

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

(defun elpaca-tests-menu (request &optional item)
  "A test menu. REQUEST and optional ITEM."
  (let ((recipes '((:package "a" :host gitlab :repo "a/a")
                   ( :package "elpaca" :host github :repo "progfolio/elpaca"
                     :pre-build ("./pre-build")))))
    (pcase request
      ('index  (let ((items (mapcar (lambda (recipe) (cons
                                                     (intern-soft (plist-get recipe :package))
                                                     (list :source "test-menu"
                                                           :recipe recipe)))
                                   recipes)))
                 (if item (alist-get item items) items)))
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
      '(:source "test-menu"
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

(defun elpaca-tests--make-init-queue (&optional elpacas)
  "Return an init queue containing ELPACAS."
  (elpaca-q<-create :type 'init :id 0 :elpacas elpacas))

(ert-deftest elpaca-recipe ()
  "Compute recipe from ORDER."
  (let ((elpaca-recipe-functions nil))
    ;; no inheritance, full spec
    (should (elpaca-tests--plist-equal-p
             '(:package "elpaca" :host github :repo "progfolio/elpaca" :inherit nil)
             (elpaca-recipe '(elpaca :host github :repo "progfolio/elpaca" :inherit nil))))
    ;; basic default inheritance
    (let ((elpaca-order-functions '(elpaca-tests--no-inheritance)))
      (should (elpaca-tests--plist-equal-p
               '(:package "elpaca" :host github :repo "progfolio/elpaca" :inherit nil)
               (elpaca-recipe '(elpaca :host github :repo "progfolio/elpaca")))))
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

(ert-deftest elpaca-resolve-resumes-pre-step-waiters ()
  "Resolving the last condition should continue pre-step waiters."
  (let* ((key '(finished . elpaca))
         (elpaca--conditions (make-hash-table :test #'equal))
         (continued nil)
         (status-changes nil)
         (e (elpaca<--create :id 'elpaca :conditions (list key))))
    (puthash key (list e) elpaca--conditions)
    (cl-letf (((symbol-function 'elpaca-continue)
               (lambda (waiter) (setq continued waiter)))
              ((symbol-function 'elpaca--set-status)
               (lambda (waiter status) (push (cons waiter status) status-changes)))
              ((symbol-function 'elpaca-note) #'ignore))
      (elpaca-resolve 'finished 'elpaca))
    (should (eq continued e))
    (should-not status-changes)
    (should-not (elpaca<-conditions e))
    (should-not (gethash key elpaca--conditions))))

(ert-deftest elpaca-finalize-queue-defers-after-init-until-emacs-finishes-init ()
  "Do not run `elpaca-after-init-hook' before Emacs sets `after-init-time'."
  (let* ((after-init-time nil)
         (elpaca-after-init-time nil)
         (elpaca--waiting nil)
         (elpaca-post-queue-hook nil)
         (elpaca--post-queues-hook nil)
         (elpaca-queue-start-functions nil)
         (elpaca-queue-finish-functions nil)
         (hook-ran nil)
         (elpaca-after-init-hook (list (lambda () (setq hook-ran t))))
         (q (elpaca-tests--make-init-queue)))
    (let ((elpaca--queues (list q)))
      (elpaca--finalize-queue q))
    (should-not hook-ran)
    (should-not elpaca-after-init-time)))

(ert-deftest elpaca-process-queues-defers-after-init-until-emacs-finishes-init ()
  "Do not run `elpaca-after-init-hook' from the fallback path before init ends."
  (let* ((after-init-time nil)
         (elpaca-after-init-time nil)
         (elpaca--waiting nil)
         (elpaca--debug-init nil)
         (elpaca--post-queues-hook nil)
         (hook-ran nil)
         (elpaca-after-init-hook (list (lambda () (setq hook-ran t))))
         (elpaca--queues (list (elpaca-tests--make-init-queue))))
    (elpaca-process-queues)
    (should-not hook-ran)
    (should-not elpaca-after-init-time)))

(provide 'elpaca-tests)
;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; elpaca-tests.el ends here
