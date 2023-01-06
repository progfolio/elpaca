;;; elpaca-test.el --- Testing macro for Elpaca      -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Nicholas Vollmer

;; Author: Nicholas Vollmer
;; Keywords: convenience

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

;; `elpaca-test' is a declarative macro aimed at making it easy to create and share bug reproduction enviornments.
;; Example usages:
;;
;; Installs Elpaca in a temporary environment. Launches an Emacs instance with doc/init.el
;; (elpaca-test)
;;
;; As above, but installs Elpaca and checks out the "feat/example" branch first.
;; (elpaca-test :ref "feat/example")
;;
;; As above, but copies over user's init.el
;; (elpaca-test :ref "feat/example" :init (:file "~/.emacs.d/init.el"))
;;
;; As above, with forms replacing everything after bootstrap snippet in doc/init.el
;; (elpaca-test :ref "feat/example" :init (elpaca one) (elpaca two))

;; @TODO: Once elpaca-wait installed, re-implement to use yodel?

;;; Code:
(require 'elpaca)
(defvar elpaca-test--keywords '(:ref :dir :init :early-init :keep :name))
(eval-and-compile
  (defun elpaca-test--args (body)
    "Return arg plist from BODY."
    (cl-loop with (acc results)
             for form in (reverse body)
             for flushp = (keywordp form)
             when (and flushp (not (memq form elpaca-test--keywords)))
             do (user-error "Unrecognized keyword %s" form)
             do (if (not flushp)
                    (push form acc)
                  (push acc results)
                  (setq acc nil)
                  (push form results))
             finally do (when acc (user-error "Missing first keyword"))
             finally return results)))

;;;###autoload
(defmacro elpaca-test (&rest body)
  "Test Elpaca in a clean environment.
BODY is psuedo plist which allows multiple values for certain keys.
The following keys are recognized:
  :name description of the test

  :ref git ref to check out or `local' to use local copy in current repo state

  :dir `user-emacs-directory' name expanded in `temporary-file-irectory'.
    Only relative paths are accepted.

  :init (:file \"path/to/init.el\") or forms...
    Content of the init.el file

  :early-init (:file \"path/to/early-init.el\") or forms...
    Content of the early-init.el file

  :keep t or a list containing any of the following symbols:
    `builds'
    `cache'
    `repos'
   If t, the directory is left in the state it was in as of last run.
   If a symbol list, those specific `elpaca-directory' folders are not removed."
  (declare (indent 0))
  (let* ((args (elpaca-test--args body))
         (keep (car (plist-get args :keep)))
         (keep-all-p (eq keep t))
         (keep-builds-p (or keep-all-p (member 'builds keep)))
         (keep-cache-p  (or keep-all-p (member 'cache  keep)))
         (keep-repos-p  (or keep-all-p (member 'repos  keep)))
         (init (plist-get args :init))
         (init-filep (eq (car-safe (car-safe init)) :file))
         (early (plist-get args :early-init))
         (early-filep (eq (car-safe (car-safe early)) :file))
         (ref (car (plist-get args :ref)))
         (localp (eq ref 'local))
         (test-builds "./elpaca/builds/")
         (test-repos "./elpaca/repos/")
         (test-cache "./elpaca/cache/")
         (dir (car (plist-get args :dir))))
    (when-let ((dir)
               (expanded (file-name-as-directory (expand-file-name dir))))
      (when (equal expanded (file-name-as-directory dir)) (user-error ":dir must be relative path"))
      (when (equal expanded (expand-file-name user-emacs-directory))
        (user-error ":dir cannot be user-emacs-directory")))
    (when-let (((listp keep))
               (err (cl-remove-if (lambda (el) (memq el '(builds cache repos))) keep)))
      (user-error "Unknown :keep value %S" err))
    `(let ((default-directory ,@`(,(if dir
                                       `(expand-file-name ,dir temporary-file-directory)
                                     '(make-temp-file "elpaca." 'directory)))))
       (unless (file-exists-p default-directory) (make-directory default-directory 'parents))
       ;;delete unwanted files/directories
       (dolist (path ',(delq nil `(,(unless keep-builds-p test-builds)
                                   ,(unless keep-repos-p  test-repos)
                                   ,(unless keep-cache-p  test-cache)
                                   "./early-init.el"
                                   "./init.el"
                                   ,@(and localp '("./elpaca/repos/elpaca"
                                                   "./elpaca/builds/elpaca")))))
         (when-let ((expanded (expand-file-name path)))
           (if (file-directory-p expanded)
               (delete-directory expanded 'recursive)
             (delete-file expanded))))
       ;;install local repo/build/cache
       ,@(when localp
           '((dolist (path '("./repos/elpaca" "./builds/elpaca" "./cache/"))
               (when-let ((local (expand-file-name path elpaca-directory))
                          ((file-exists-p local)))
                 (copy-directory local (expand-file-name path (expand-file-name "./elpaca/"))
                                 nil 'parents 'copy-contents)))))
       ,@(when early
           `((with-temp-buffer
               ,@(if early-filep
                     `((insert-file-contents (expand-file-name ,(cadar early))))
                   `((insert ,(pp-to-string `(progn ,@early)))))
               (write-file (expand-file-name "./early-init.el")))))
       (with-temp-buffer
         ,@`(,(cond
               (init-filep `(insert-file-contents (expand-file-name ,(cadar init))))
               (localp '(insert-file-contents
                         (expand-file-name "./repos/elpaca/doc/init.el" elpaca-directory)))
               ;;@TODO :repo arg which alllows to specify different URL.
               ;; Would also need to search/replace :repo in demo init.
               (t `(let ((url ,(format "https://raw.githubusercontent.com/progfolio/elpaca/%s/doc/init.el" (or ref "master"))))
                     (defvar url-http-end-of-headers)
                     (defvar url-http-response-status)
                     (insert
                      (with-current-buffer (url-retrieve-synchronously url nil 'inhibit-cookies)
                        (unless (equal url-http-response-status 200)
                          (error "Unable to download %S %S" url
                                 url-http-response-status))
                        (delete-region (point-min) url-http-end-of-headers)
                        (buffer-string)))))))
         ,@(unless (or (null ref) init-filep localp)
             `((goto-char (point-min))
               (re-search-forward ":ref nil")
               (replace-match (format ":ref %S" ,ref))))
         ,@(when (and init (not init-filep))
             `((goto-char (point-max))
               (forward-line -1)
               (re-search-backward "^;; Install" nil 'noerror)
               (delete-region (point) (point-max))
               (insert ,(pp-to-string `(progn ,@init)))))
         (write-file (expand-file-name "./init.el")))
       (make-process
        :name "elpaca-test"
        :command (list ,(elpaca--emacs-path)
                       "--debug-init"
                       ,@(if (version< emacs-version "29")
                             `("-q" ; Approximate startup.el sequence
                               "--eval" "(setq debug-on-error t)"
                               "--eval" (format "(setq user-emacs-directory %S)" default-directory)
                               "-l" "./early-init.el"
                               "--eval" "(run-hooks 'before-init-hook)"
                               "-l" "./init.el"
                               "--eval" "(setq after-init-time (current-time))"
                               "--eval" "(run-hooks 'after-init-hook)"
                               "--eval" "(run-hooks 'emacs-startup-hook)")
                           '((format "--init-directory=%s" default-directory)))))
       (message "testing Elpaca @ %s in %s"
                ,@(or (and ref (not localp) (list ref))
                      `((let ((default-directory (expand-file-name "repos/elpaca/" elpaca-directory)))
                          (concat (and ,localp (or (ignore-errors (elpaca-process-output "git" "diff" "--quiet")) "DIRTY "))
                                  (string-trim (elpaca-process-output "git" "log" "--pretty=%h %D" "-1"))))))
                default-directory))))

(provide 'elpaca-test)
;;; elpaca-test.el ends here
