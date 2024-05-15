;;; elpaca-test.el --- Elpaca debugging/testing macro      -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Nicholas Vollmer

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
(defvar elpaca-test--keywords
  '(:args :before :dir :early-init :init :keep :name :ref :depth :interactive :timeout :buffer))
(defvar elpaca-test--dirs nil "List of directories created by `elpaca-test'.")

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
           ((or (not (equal expanded (expand-file-name user-emacs-directory)))
                (user-error ":dir cannot be user-emacs-directory"))))
      (if (equal name expanded) expanded (expand-file-name name temporary-file-directory))
    (expand-file-name (make-temp-name "elpaca.") temporary-file-directory)))

(defun elpaca-test--ensure-dir (dir args)
  "Ensure user wants to run test in DIR. ARGS :keep may be overridden."
  (let ((dirp (file-exists-p dir))
        (warning (format "%S not created by elpaca-test. Run test in this directory?" dir)))
    (cond
     ((not dirp) (cl-pushnew dir elpaca-test--dirs :test #'equal))
     ((not (or (member dir elpaca-test--dirs) (yes-or-no-p warning)))
      (user-error "Elpaca test aborted"))
     (t (setq args (plist-put args :keep '(t)))))))

(defvar url-http-end-of-headers)
(defvar url-http-response-status)
(defconst elpaca-test--upstream-format
  "https://raw.githubusercontent.com/progfolio/elpaca/%s/doc/init.el"
  "Format string for upstream URL. @TODO: don't hardcode this.")

(defun elpaca-test--init (string order &optional installerp)
  "Return modified init.el STRING according to ORDER.
If INSTALLERP is non-nil, stop after Elpaca installer."
  (let (sexp form body)
    (condition-case err
        (while (setq sexp (read-from-string string (or (cdr sexp) 0))
                     form (car (push (car sexp) body)))
          (cond ((and (eq (car-safe form) 'defvar)
                      (eq (car (cdr-safe form)) 'elpaca-order))
                 (setf (nth 2 form)
                       `'(elpaca ,@(elpaca-merge-plists (cdr (eval (nth 2 form) t)) order))))
                ((and installerp (eq (car-safe form) 'elpaca))
                 (signal 'end-of-file nil))))
      ((end-of-file) nil)
      ((error) (error (car err) (cdr err))))
    (mapconcat #'pp-to-string (nreverse body))))

(defun elpaca-test--upstream-init (&optional ref)
  "Return upstream init.el file for REF."
  (let ((url (format elpaca-test--upstream-format (or ref "master"))))
    (with-current-buffer (url-retrieve-synchronously url 'silent 'inhibit-cookies)
      (unless (equal url-http-response-status 200)
        (error "Unable to download %S %S" url url-http-response-status))
      (delete-region (point-min) url-http-end-of-headers)
      (string-trim (buffer-substring-no-properties (point-min) (point-max))))))

(defun elpaca--test-write-init (file ref depth forms)
  "Write init.el FILE with FORMS in test environment.
If FILE is nil, use upstream demo init file determined by REF.
For DEPTH and FORMS see `elpaca-test' :depth and :init."
  (let ((contents (if file (with-temp-buffer (insert-file-contents (expand-file-name file))
                                             (buffer-string))
                    (elpaca-test--upstream-init ref))))
    (elpaca--write-file (expand-file-name "./init.el")
      (emacs-lisp-mode)
      (insert (elpaca-test--init contents `(:ref ,(unless (eq ref 'local) ref) :depth ,depth) forms))
      (mapc #'print forms)
      (elisp-enable-lexical-binding))))

(defun elpaca-test--write-early-init (file forms)
  "Write test environment early-init.el FILE with FORMS."
  (elpaca--write-file (expand-file-name "./early-init.el")
    (if file (insert-file-contents (expand-file-name file))
      (emacs-lisp-mode)
      (mapc #'print forms)
      (elisp-enable-lexical-binding))))

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
           for path in '("./repos/elpaca" "./cache/")
           do (when-let ((local (expand-file-name path elpaca-directory))
                         ((file-exists-p local)))
                (elpaca-test--copy-dir local (expand-file-name path env)))))

(defun elpaca-test--display (vars)
  "Display test with VARS when test finished and Emacs idle."
  (unless (plist-get vars :interactive)
    (run-with-idle-timer 1 nil (lambda (b) (pop-to-buffer b) (goto-char (point-min)))
                         (current-buffer))))

(defun elpaca-test--format (vars)
  "Finish formatting test with VARS."
  (unless (plist-get vars :interactive)
    (insert "```\n</details>")
    (goto-char (point-min))
    (let ((standard-output (current-buffer))
          print-circle print-length)
      (princ "<!-- copy buffer contents to issue comment or new issue -->\n")
      (princ "<details open><summary>Test Case</summary>\n\n")
      (princ "[How to run this test?]\
(https://github.com/progfolio/elpaca/wiki/Troubleshooting#the-elpaca-test-macro)\n")
      (princ "\n```emacs-lisp\n")
      (princ (elpaca-test--form (nthcdr 2 vars)))
      (princ "```\n\n</details>\n<details><summary>Host Env</summary>\n\n<table>\n")
      (cl-loop for (cat . info) in (elpaca-version)
               do (princ (format "<tr><td>%s</td><td>%s</td>\n" cat
                                 (string-trim (replace-regexp-in-string "\n" "" (format "%s" info))))))
      (princ "</table>\n</details>\n\n<details><summary>Output</summary>\n\n```emacs-lisp\n"))
    (when (fboundp 'markdown-mode) (markdown-mode))))

(defcustom elpaca-test-finish-functions (list #'elpaca-test--format #'elpaca-test--display)
  "Abnormal hook run when test sentinel is finished.
Each function is called with the test declaration's arguments list.
When the test is non-interactive, its process buffer is initially current."
  :type 'hook :group 'elpaca)

(defun elpaca-test--announce (args)
  "Print test message for test with ARGS."
  (let ((localp (equal (car (plist-get args :ref)) 'local)))
    (run-with-timer
     0 nil `(lambda () ;;@HACK: Show message when evaluating interactively
              (message "Testing Elpaca in %s @ %s"
                       ,default-directory
                       (if-let ((localp ,localp)
                                (default-directory (expand-file-name "repos/elpaca/" elpaca-directory)))
                           (concat (or (ignore-errors (elpaca-process-output "git" "diff" "--quiet")) "DIRTY ")
                                   (string-trim (elpaca-process-output "git" "log" "--pretty=%h %D" "-1")))
                         ,(or (unless localp (car (plist-get args :ref))) "master")))))))

(defcustom elpaca-test-start-functions (list #'elpaca-test--announce)
  "Abnormal hook run just before test is started.
Each function is called with the test declaration's arguments list."
  :type 'hook :group 'elpaca)

(defun elpaca-test--sentinel (process _)
  "Prepare post-test PROCESS buffer output, display, test environment.
If DELETE is non-nil, delete test environment."
  (when-let (((member (process-status process) '(exit signal failed)))
             (vars (process-get process :vars)))
    (when-let (((not (car-safe (plist-get vars :keep))))
               (dir (plist-get vars :computed-dir)))
      (message "Removing Elpaca test env: %S" dir)
      (delete-directory dir 'recursive))
    (with-current-buffer (if-let ((buffer (process-buffer process))
                                  ((buffer-live-p buffer)))
                             buffer
                           (current-buffer))
      (run-hook-with-args 'elpaca-test-finish-functions vars))))

;;;###autoload
(defun elpaca-test-timeout ()
  "Cancel pending orders."
  (dolist (e (mapcar #'cdr (elpaca--queued)))
    (unless (memq (elpaca--status e) '(finished failed)) (elpaca--fail e "Test timeout"))))

(defun elpaca-test--command (&optional args batch timeout early)
  "Return process command list with ARGS.
BATCH, TIMEOUT, and EARLY match :interactive, :timeout, :early-init keys."
  `(,(elpaca--emacs-path)
    ,@(and (not (equal args '(nil))) (or args '("--debug-init")))
    ,@(when batch '("--batch"))
    ,@(if (or batch (< emacs-major-version 29))
          `("-Q" ; Approximate startup.el sequence
            ,@(when timeout (list "--eval" (format "(run-at-time %d nil #'elpaca-test-timeout)" timeout)))
            "--eval" "(setq debug-on-error t after-init-time nil)"
            "--eval" ,(format "(setq user-emacs-directory %S)" default-directory)
            ,@(when early '("-l" "./early-init.el"))
            "--eval" "(run-hooks 'before-init-hook)"
            "-l" "./init.el"
            "--eval" "(setq after-init-time (current-time))"
            "--eval" "(run-hooks 'after-init-hook)"
            "--eval" "(run-hooks 'emacs-startup-hook)")
        `(,(format "--init-directory=%s" default-directory)))))

(defun elpaca-test--make-process (name buffer command vars)
  "Return subprocess with NAME BUFFER VARS executing COMMAND."
  (process-put (make-process :name name :buffer buffer :command command
                             :sentinel #'elpaca-test--sentinel)
               :vars vars))

;;;###autoload
(defmacro elpaca-test (&rest body)
  "Test Elpaca in a clean environment.
BODY is a plist which allows multiple values for certain keys.
The following keys are recognized:
  :name description of the test

  :ref git ref to check out or `local' to use local copy in current repo state

  :depth number of Elpaca repository commits to clone

  :dir `user-emacs-directory' name.
    Expanded in temporary filedirectory if it is a relative path or nil.
    Otherwise, the absolute file path is used.

  :init `user', (:file \"path/to/init.el\") or forms...
    Content of the init.el file.
    `user' is shorthand for `user-emacs-diretory'/init.el.

  :early-init Content of the init.el file. Accepts same args as :init.

  :interactive t or nil. When non-nil, start an interactive Emacs session.

  :args String... Emacs subprocess command line args

  :keep t or nil. When non-nil, prevent test environment deletion after test

  :timeout N. A number or seconds to wait for package installations to complete.
              Pending orders are failed after this time.
  :buffer STRING. Name of the process buffer. Ignored when :interactive."
  (declare (indent 0))
  (unless lexical-binding (user-error "Lexical binding required for elpaca-test"))
  (let* ((args (elpaca-test--args body))
         (batchp (not (car (plist-get args :interactive))))
         (timeout (car (plist-get args :timeout)))
         (init (plist-get args :init))
         (ref (car (plist-get args :ref)))
         (depth (if-let ((declared (plist-member args :depth))) (caadr declared) 1))
         (localp (eq ref 'local))
         (init-file (cond ((eq (car-safe (car-safe init)) :file)
                           (when localp (user-error "Cannot use :ref local with :init (:file ...)"))
                           (eval (cadar init) t))
                          ((eq (car-safe init) 'user) (locate-user-emacs-file "./init.el"))
                          (localp (expand-file-name "./repos/elpaca/doc/init.el" elpaca-directory))))
         (early (plist-get args :early-init))
         (early-file (cond ((eq (car-safe (car-safe early)) :file) (eval (cadar early) t))
                           ((eq (car-safe early) 'user) (locate-user-emacs-file "./early-init.el"))))
         (procname (make-symbol "procname"))
         (argsym (make-symbol "args"))
         (dir (elpaca-test--dir (car (plist-get args :dir))))
         print-length print-circle print-level
         eval-expression-print-level eval-expression-print-length)
    (elpaca-test--ensure-dir dir args)
    (setq args (append (list :computed-dir dir) args))
    `(let* ((default-directory ,dir)
            (,procname (format "elpaca-test-%s" default-directory))
            (,argsym ',args)
            (buffer ,@(list (when batchp (or (car (plist-get args :buffer))
                                             `(generate-new-buffer ,procname))))))
       (unless (file-exists-p default-directory) (make-directory default-directory 'parents))
       ,@(when localp '((elpaca-test--copy-local-store)))
       ,@(when early `((elpaca-test--write-early-init ,early-file ',(unless early-file early))))
       (elpaca--test-write-init
        ,init-file ',ref ',depth ',(when (or localp (null init-file))
                                     (unless (equal init '(user)) init)))
       ,@(when-let ((before (plist-get args :before)))
           `((let ((default-directory default-directory)) ,@before)))
       (run-hook-with-args 'elpaca-test-start-functions ,argsym)
       (elpaca-test--make-process
        ,procname buffer
        (elpaca-test--command ',(plist-get args :args) ,batchp ,timeout ',early)
        ,argsym)
       nil)))

(provide 'elpaca-test)
;;; elpaca-test.el ends here
