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
(require 'elpaca-menu-elpa)

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

(ert-deftest elpaca-menu-elpa-dotted-version ()
  "Render package VERSION lists as dotted strings."
  (should (equal "1.2.3" (elpaca-menu-elpa-dotted-version '(1 2 3)))))

(ert-deftest elpaca-menu-elpa-upstream ()
  "Extract the upstream URL from real pair-list and flat-plist metadata."
  ;; Real ELPA form: a list of (KEYWORD . VALUE) chunks
  (should (equal "https://www.gnu.org/software/auctex/"
                 (elpaca-menu-elpa--upstream
                  '((:url . "https://www.gnu.org/software/auctex/")
                    (:keywords "tex" "latex")
                    (:commit . "cc5a4776967eeaac3bba75b3be4365695d7dc97d")))))
  ;; `:upstream' key variant
  (should (equal "UP" (elpaca-menu-elpa--upstream '((:upstream . "UP") (:commit . "c")))))
  ;; Flat-plist form (alternate serialization)
  (should (equal "URL" (elpaca-menu-elpa--upstream '(:url "URL"))))
  (should (equal "UP"  (elpaca-menu-elpa--upstream '(:upstream "UP"))))
  ;; No URL present
  (should-not (elpaca-menu-elpa--upstream '((:keywords "tex") (:commit . "c"))))
  (should-not (elpaca-menu-elpa--upstream '()))
  (should-not (elpaca-menu-elpa--upstream nil)))

(ert-deftest elpaca-menu-elpa-item-tar ()
  "Return a menu item cell with a tarball recipe for a tar package."
  (should (equal
           '(auctex . (:source "gnu"
                              :description "Description."
                              :url "https://www.gnu.org/software/auctex/"
                              :recipe
                              (:package "auctex"
                                       :repo ("https://www.gnu.org/software/auctex/" . "auctex")
                                       :url "https://elpa.gnu.org/packages/auctex-14.2.0.tar"
                                       :tar "14.2.0")))
           (elpaca-menu-elpa--item 'auctex "gnu" "https://elpa.gnu.org/packages/"
                                   '(14 2 0) "Description." 'tar
                                   '((:url . "https://www.gnu.org/software/auctex/")
                                     (:commit . "cc5a4776967eeaac3bba75b3be4365695d7dc97d"))
                                    nil))))

(ert-deftest elpaca-menu-elpa-item-single ()
  "Return a menu item cell with a single-file recipe for a single package."
  (should (equal
           '(widget . (:source "nongnu"
                              :description ""
                              :url "https://example.com/widget"
                              :recipe
                              (:package "widget"
                                       :repo ("https://example.com/widget" . "widget")
                                       :url "https://elpa.nongnu.org/nongnu/widget-0.5.1.el")))
           (elpaca-menu-elpa--item 'widget "nongnu" "https://elpa.nongnu.org/nongnu/"
                                   '(0 5 1) nil 'single
                                   '((:url . "https://example.com/widget"))
                                    nil))))

(ert-deftest elpaca-menu-elpa-item-no-upstream ()
  "Fall back to the archive page and omit :repo when upstream is unknown."
  (should (equal
           '(bare . (:source "gnu"
                            :description ""
                            :url "https://elpa.gnu.org/packages/bare.html"
                            :recipe
                            (:package "bare"
                                     :url "https://elpa.gnu.org/packages/bare-1.0.el")))
           (elpaca-menu-elpa--item 'bare "gnu" "https://elpa.gnu.org/packages/"
                                   '(1 0) nil 'single '()
                                    nil))))

(ert-deftest elpaca-menu-elpa-item-merged-packages ()
  "Merge the elpa-packages entry's keys into a flat plist recipe."
  (let ((cell (elpaca-menu-elpa--item 'compat "gnu" "https://elpa.gnu.org/packages/"
                                       '(31 1 0 0) "Description." 'tar
                                       '((:url . "https://example.org/compat"))
                                       '(compat :url "https://github.com/emacs-compat/compat"
                                               :doc "compat.texi"
                                               :branch "main"))))
    (should (plistp (plist-get (cdr cell) :recipe)))
    (should (equal (plist-get (plist-get (cdr cell) :recipe) :repo)
                  '("https://github.com/emacs-compat/compat" . "compat")))
    (should (equal (plist-get (plist-get (cdr cell) :recipe) :doc) "compat.texi"))
    (should (equal (plist-get (plist-get (cdr cell) :recipe) :branch) "main"))
    (should (equal (plist-get (plist-get (cdr cell) :recipe) :url)
                  "https://elpa.gnu.org/packages/compat-31.1.0.0.tar"))
    (should (equal (plist-get (plist-get (cdr cell) :recipe) :tar) "31.1.0.0"))))


(ert-deftest elpaca-menu-elpa-item-no-upstream-mirror ()
  "When elpa-packages :url is nil, :repo is the mirror repo and :branch the externals branch."
  (let ((elpaca-menu-elpa-use-mirror t)
          (cell (elpaca-menu-elpa--item 'persist "gnu" "https://elpa.gnu.org/packages/"
                                        '(0 8) "Persist." 'tar
                                        '((:url . "https://elpa.gnu.org/packages/persist.html"))
                                        '(persist :url nil))))
      (should (equal (plist-get (plist-get (cdr cell) :recipe) :repo)
                  '("https://github.com/emacsmirror/gnu_elpa" . "persist")))
      (should (equal (plist-get (plist-get (cdr cell) :recipe) :branch) "externals/persist"))
      (should (equal (plist-get (plist-get (cdr cell) :recipe) :url)
                    "https://elpa.gnu.org/packages/persist-0.8.tar"))
      (should (equal (plist-get (cdr cell) :url) "https://elpa.gnu.org/packages/persist.html"))))

(ert-deftest elpaca-menu-elpa-index ()
  "Fulfill the menu `index' protocol from the archive cache."
  (let* ((elpaca-cache-directory
          (expand-file-name "elpaca-menu-elpa-tests/el-pa/c/" temporary-file-directory))
         (recipe (list :package "foo"
                       :url "https://elpa.gnu.org/packages/foo-1.0.el"))
         (cache (list (cons 'foo (list :source "gnu"
                                       :description "Foo."
                                       :url "https://elpa.gnu.org/packages/foo.html"
                                       :recipe recipe)))))
    (elpaca-menu-elpa--write-cache "gnu" cache)
    (should (file-exists-p (expand-file-name "elpaca-menu-gnu.eld"
                                             elpaca-cache-directory)))
    (should (equal cache (elpaca-menu-elpa-gnu 'index)))
    (should (equal (cdr (assq 'foo cache)) (elpaca-menu-elpa-gnu 'index 'foo)))
    (should-not (elpaca-menu-elpa-gnu 'index 'bar))))

(ert-deftest elpaca-menu-melpa-item-repo ()
  "Bare `:repo' + `:fetcher' from a recipes.json entry."
  (should (equal
           '(0blayout . (:source "melpa"
                               :description "Layout grouping with ease"
                               :url "https://melpa.org/packages/0blayout.html"
                               :recipe
                               (:package "0blayout"
                                        :repo "etu/0blayout-mode"
                                        :fetcher github)))
           (elpaca-menu-melpa--item '0blayout "melpa" "https://melpa.org/packages/"
                                    '((repo . "etu/0blayout-mode")
                                      (fetcher . "github"))
                                    "Layout grouping with ease"))))

(ert-deftest elpaca-menu-melpa-item-sourcehut ()
  "`:fetcher' is the entry's fetcher (sourcehut)."
  (should (equal
           '(elpaca . (:source "melpa"
                              :description ""
                              :url "https://melpa.org/packages/elpaca.html"
                              :recipe
                              (:package "elpaca"
                                       :repo "progfolio/elpaca"
                                       :fetcher sourcehut)))
           (elpaca-menu-melpa--item 'elpaca "melpa" "https://melpa.org/packages/"
                                    '((repo . "progfolio/elpaca")
                                      (fetcher . "sourcehut"))
                                    nil))))

(ert-deftest elpaca-menu-melpa-item-url ()
  "`:repo' from the entry's `url' + `:fetcher'."
  (should (equal
           '(foo . (:source "melpa"
                           :description ""
                           :url "https://depp.brause.cc/foo"
                           :recipe
                           (:package "foo"
                                    :repo "https://depp.brause.cc/foo"
                                    :fetcher git)))
           (elpaca-menu-melpa--item 'foo "melpa" "https://melpa.org/packages/"
                                    '((url . "https://depp.brause.cc/foo")
                                      (fetcher . "git"))
                                    nil))))

(ert-deftest elpaca-menu-melpa-item-files ()
  "`:files' is read from the entry's form."
  (should (equal
           '(750words . (:source "melpa"
                                :description ""
                                :url "https://melpa.org/packages/750words.html"
                                :recipe
                                (:package "750words"
                                         :repo "zzamboni/750words-client"
                                         :fetcher github
                                         :files ("750words.el"))))
           (elpaca-menu-melpa--item '750words "melpa" "https://melpa.org/packages/"
                                    '((fetcher . "github")
                                      (repo . "zzamboni/750words-client")
                                      (files . "(\"750words.el\")"))
                                    nil))))

(ert-deftest elpaca-menu-melpa-index ()
  "Fulfill the menu `index' protocol for MELPA from the archive cache."
  (let* ((elpaca-cache-directory
          (expand-file-name "elpaca-menu-melpa-tests/" temporary-file-directory))
         (recipe (list :package "foo"
                       :repo "https://github.com/x/foo"))
         (cache (list (cons 'foo (list :source "melpa"
                                       :description "Foo."
                                       :url "https://github.com/x/foo"
                                       :recipe recipe)))))
    (elpaca-menu-elpa--write-cache "melpa" cache)
    (should (file-exists-p (expand-file-name "elpaca-menu-melpa.eld"
                                             elpaca-cache-directory)))
    (should (equal cache (elpaca-menu-melpa 'index)))
    (should (equal (cdr (assq 'foo cache)) (elpaca-menu-melpa 'index 'foo)))
    (should-not (elpaca-menu-melpa 'index 'bar))))

(ert-deftest elpaca-menu-melpa-source-fn ()
  "elpaca-menu-melpa passes a source-fn (not an item-fn) to `--index'."
  (let ((elpaca-cache-directory
         (expand-file-name "elpaca-menu-melpa-srctest/" temporary-file-directory))
        (called nil)
        (orig (symbol-function 'elpaca-menu-melpa--source)))
    ;; Pre-seed the cache so the first call (which triggers the autoload of
    ;; elpaca-menu-melpa and re-loads this file) reads the cache instead of
    ;; fetching, and leaves elpaca-menu-melpa defined.
    (elpaca-menu-elpa--write-cache "melpa"
                                   (list (cons 'seed (list :source "melpa"
                                                          :recipe (list :package "seed")))))
    (elpaca-menu-melpa 'index)
    ;; Now elpaca-menu-melpa is defined; stub the source-fn and clear the
    ;; cache so the next call goes through the (stubbed) source-fn.
    (setf (symbol-function 'elpaca-menu-melpa--source)
          (lambda (name root)
            (setq called (list name root))
            (list (cons 'foo (list :source "melpa"
                                   :recipe (list :package "foo"))))))
    (delete-file (expand-file-name "elpaca-menu-melpa.eld" elpaca-cache-directory))
    (unwind-protect
         (elpaca-menu-melpa 'index)
      (setf (symbol-function 'elpaca-menu-melpa--source) orig))
    (should (equal called '("melpa" "https://melpa.org/packages/")))))

(provide 'elpaca-tests)
;; Local Variables:
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; elpaca-tests.el ends here
