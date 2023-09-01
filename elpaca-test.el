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

;; `elpaca-test' is a declarative macro aimed at making it easy to create and share bug reproduction environments.
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
;; As above, with forms replacing everything after the installer in doc/init.el
;; (elpaca-test :ref "feat/example" :init (elpaca one) (elpaca two))

;;; Code:
(require 'elpaca)
(require 'url)
(defvar elpaca-test--keywords '(:args :before :dir :early-init :init :keep :name :ref :interactive))

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
           finally return results))

(defun elpaca-test--form (args)
  "Return test form string from ARGS."
  (let ((form `(elpaca-test ,@(cl-loop for (k v) on args by #'cddr append `(,k ,@v)))))
    (with-temp-buffer
      (if (fboundp 'pp-emacs-lisp-code)
          (pp-emacs-lisp-code form)
        (insert (pp-to-string form)))
      (buffer-string))))

(defun elpaca-test--dir (&optional name)
  "Return valid test directory from NAME.
Creates a temporary dir if NAME is nil."
  (if-let ((name)
           (expanded (file-name-as-directory (expand-file-name name)))
           ((or (not (equal expanded (file-name-as-directory name)))
                (user-error ":dir must be relative path")))
           ((or (not (equal expanded (expand-file-name user-emacs-directory)))
                (user-error ":dir cannot be user-emacs-directory"))))
      (expand-file-name name temporary-file-directory)
    (make-temp-file "elpaca." 'directory)))

(defvar url-http-end-of-headers)
(defvar url-http-response-status)
(defconst elpaca-test--upstream-format
  "https://raw.githubusercontent.com/progfolio/elpaca/%s/doc/init.el"
  "Format string for upstream URL. @TODO: don't hardcode this.")
(defun elpaca-test--upstream-init (&optional ref)
  "Return upstream REF's init.el body as a string."
  (let ((url (format elpaca-test--upstream-format (or ref "master"))))
    (with-current-buffer (url-retrieve-synchronously url nil 'inhibit-cookies)
      (unless (equal url-http-response-status 200)
        (error "Unable to download %S %S" url url-http-response-status))
      (delete-region (point-min) url-http-end-of-headers)
      (string-trim (buffer-substring-no-properties (point-min) (point-max))))))

(defun elpaca-test--copy-dir (dir dest)
  "Copy DIR to DEST, resolving symlinks so files in DEST do not point to DIR."
  (setq dir (expand-file-name dir)
        dest (expand-file-name dest))
  (copy-directory dir dest 'keep-time 'parents 'copy-contents)
  (cl-loop for f in (directory-files-recursively dest ".*" 'dirs nil)
           do (when-let  (((file-symlink-p f))
                          (truename (file-truename f)))
                (delete-file f)
                (copy-file truename f 'overwrite))))

(defun elpaca-test--copy-local-store ()
  "Copy host `elpaca-directory' store to test env."
  (cl-loop with env = (expand-file-name "./elpaca/")
           for path in '("./repos/elpaca" "./builds/elpaca" "./cache/")
           do (when-let ((local (expand-file-name path elpaca-directory))
                         ((file-exists-p local)))
                (elpaca-test--copy-dir local (expand-file-name path env)))))

(defun elpaca-test--format-output-buffer (buffer test)
  "Format TEST output BUFFER ."
  (let ((standard-output (get-buffer-create buffer))
        print-circle print-length)
    (princ "<!-- copy buffer contents to issue comment or new issue -->\n")
    (princ "<details open><summary>Test Case</summary>\n\n")
    (princ "[How to run this test?]\
(https://github.com/progfolio/elpaca/wiki/Troubleshooting#the-elpaca-test-macro)\n")
    (princ "\n```emacs-lisp\n")
    (princ test)
    (princ "```\n\n</details>\n<details><summary>Host Env</summary>\n\n<table>\n")
    (cl-loop for (cat . info) in (elpaca-version)
             do (princ (format "<tr><td>%s</td><td>%s</td>\n" cat
                               (string-trim (replace-regexp-in-string "\n" "" (format "%s" info))))))
    (princ "</table>\n</details>\n\n<details><summary>Output</summary>\n\n```emacs-lisp\n")))

(defun elpaca-test--sentinel (process _)
  "Prepare post-test PROCESS buffer output, display, test environment.
If DELETE is non-nil, delete test environment."
  (when (member (process-status process) '(exit signal failed))
    (when-let ((vars (process-get process :vars))
               ((not (car-safe (plist-get vars :keep))))
               (dir (plist-get vars :computed-dir)))
      (message "Removing Elpaca test env: %S" dir)
      (delete-directory dir 'recursive))
    (when-let ((buffer (process-buffer process))
               ((buffer-live-p buffer)))
      (with-current-buffer buffer (insert "```\n</details>")
                           (when (fboundp 'markdown-mode) (markdown-mode)))
      (run-with-idle-timer 1 nil (lambda () (pop-to-buffer buffer) (goto-char (point-min)))))))

(declare-function elpaca-log "elpaca-log")
;;;###autoload
(defun elpaca-test-log (&rest queries)
  "Print `elpaca-log' QUERIES."
  (dolist (q queries)
    (with-current-buffer (elpaca-log q)
      (message "elpaca-log: %S\n%S" q
               (buffer-substring-no-properties
                (save-excursion (goto-char (point-min)) (line-end-position))
                (point-max))))))

;;;###autoload
(defmacro elpaca-test (&rest body)
  "Test Elpaca in a clean environment.
BODY is a plist which allows multiple values for certain keys.
The following keys are recognized:
  :name description of the test

  :ref git ref to check out or `local' to use local copy in current repo state

  :dir `user-emacs-directory' name expanded in the temporary file directory.

  :init `user', (:file \"path/to/init.el\") or forms...
    Content of the init.el file.
    `user' is shorthand for `user-emacs-diretory'/init.el.

  :early-init Content of the init.el file. Accepts same args as :init.

  :interactive t or nil. When non-nil, start an interactive Emacs session.

  :args String... Emacs subprocess command line args

  :keep t or nil. When non-nil, prevent test environment deletion after test."
  (declare (indent 0))
  (unless lexical-binding (user-error "Lexical binding required for elpaca-test"))
  (let* ((args (elpaca-test--args body))
         (batchp (not (car (plist-get args :interactive))))
         (test (and batchp (elpaca-test--form args)))
         (init (plist-get args :init))
         (init-filep (or (and (eq (car-safe (car-safe init)) :file) 'file)
                         (and (eq (car-safe init) 'user) 'user)))
         (early (plist-get args :early-init))
         (early-filep (or (and (eq (car-safe (car-safe early)) :file) 'file)
                          (and (eq (car-safe early) 'user) 'user)))
         (ref (car (plist-get args :ref)))
         (localp (eq ref 'local))
         (procname (make-symbol "procname"))
         print-length print-circle print-level
         eval-expression-print-level eval-expression-print-length)
    `(let* ((default-directory (elpaca-test--dir ,(car (plist-get args :dir))))
            (,procname (format "elpaca-test-%s" default-directory))
            (buffer ,@(if batchp `((generate-new-buffer ,procname)) '(nil))))
       (unless (file-exists-p default-directory) (make-directory default-directory 'parents))
       ,@(when localp '((elpaca-test--copy-local-store)))
       ,@(when early `((with-temp-file (expand-file-name "./early-init.el")
                         ,@(if early-filep
                               `((insert-file-contents
                                  (expand-file-name ,(if (eq early-filep 'user)
                                                         (locate-user-emacs-file "./early-init.el")
                                                       (cadar early)))))
                             `((insert ,(pp-to-string `(progn ,@early))))))))
       (with-temp-file (expand-file-name "./init.el")
         ,@`(,(cond
               (init-filep `(insert-file-contents (expand-file-name ,(if (eq init-filep 'user)
                                                                         (locate-user-emacs-file "./init.el")
                                                                       (cadar init)))))
               (localp '(insert-file-contents
                         (expand-file-name "./repos/elpaca/doc/init.el" elpaca-directory)))
               ;;@TODO :repo arg which alllows to specify different URL.
               ;; Would also need to search/replace :repo in demo init.
               (t `(insert (elpaca-test--upstream-init ,ref)))))
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
         (elisp-enable-lexical-binding))
       ,@(when-let ((before (plist-get args :before)))
           `((let ((default-directory default-directory)) ,@before)))
       (when buffer (elpaca-test--format-output-buffer buffer ,test))
       (process-put
        (make-process
         :name ,procname
         :buffer buffer
         :command (list ,(elpaca--emacs-path)
                        ,@(if-let ((args (plist-get args :args)))
                              (unless (equal args '(nil)) args)
                            '("--debug-init"))
                        ,@(if batchp '("--batch"))
                        ,@(if (or batchp (< emacs-major-version 29))
                              `("-Q" ; Approximate startup.el sequence
                                "--eval" "(setq debug-on-error t after-init-time nil)"
                                "--eval" (format "(setq user-emacs-directory %S)" default-directory)
                                ,@(when early '("-l" "./early-init.el"))
                                "--eval" "(run-hooks 'before-init-hook)"
                                "-l" "./init.el"
                                "--eval" "(setq after-init-time (current-time))"
                                "--eval" "(run-hooks 'after-init-hook)"
                                "--eval" "(run-hooks 'emacs-startup-hook)"
                                "--eval" "(message \"\n Test Env\n\")"
                                "--eval" "(elpaca-version 'message)")
                            '((format "--init-directory=%s" default-directory))))
         :sentinel #'elpaca-test--sentinel)
        :vars `(:computed-dir ,default-directory ,@',args))
       (message "Testing Elpaca @ %s in %s"
                ,@(if localp
                      `((let ((default-directory (expand-file-name "repos/elpaca/" elpaca-directory)))
                          (concat (or (ignore-errors (elpaca-process-output "git" "diff" "--quiet")) "DIRTY ")
                                  (string-trim (elpaca-process-output "git" "log" "--pretty=%h %D" "-1")))))
                    `(,(or ref "master")))
                default-directory))))

(provide 'elpaca-test)
;;; elpaca-test.el ends here
