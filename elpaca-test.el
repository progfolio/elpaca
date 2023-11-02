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
(defvar elpaca-test--keywords '(:args :before :dir :early-init :init :keep :name :ref :interactive :timeout))
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

(defun elpaca-test--replace-init-forms (forms)
  "Replace demo init forms with FORMS."
  (goto-char (point-max))
  (forward-line -1)
  (if (not (re-search-backward "^;; Install" nil 'noerror))
      (user-error "Unable to find form marker in init file")
    (delete-region (point) (point-max))
    (dolist (form forms) (insert (pp-to-string form)))))

(defun elpaca-test--replace-init-ref (ref)
  "Replace elpaca-order's :ref with REF."
  (save-excursion (goto-char (point-min))
                  (or (re-search-forward "elpaca-order" nil t)
                      (user-error "Unable to locate elpaca-order in init file"))
                  (or (re-search-forward ":ref nil" nil t)
                      (user-error "Unable to replace :ref in init file"))
                  (replace-match (format ":ref %S" ref))))

(defun elpaca-test--upstream-init (&optional ref)
  "Create and return upstream REF's init.el file."
  (let ((url (format elpaca-test--upstream-format (or ref "master"))))
    (with-current-buffer (url-retrieve-synchronously url 'silent 'inhibit-cookies)
      (unless (equal url-http-response-status 200)
        (error "Unable to download %S %S" url url-http-response-status))
      (delete-region (point-min) url-http-end-of-headers)
      (when ref (elpaca-test--replace-init-ref ref))
      (string-trim (buffer-substring-no-properties (point-min) (point-max))))))

(defun elpaca--test-write-init (file ref forms)
  "Write init.el FILE with FORMS in test environment.
If FILE is nil, use upstream demo init file determined by REF."
  (elpaca--write-file (expand-file-name "./init.el")
    (if file
        (insert-file-contents (expand-file-name file))
      (insert (elpaca-test--upstream-init ref)))
    (when forms (elpaca-test--replace-init-forms forms))
    (elisp-enable-lexical-binding)))

(defun elpaca-test--write-early-init (file forms)
  "Write test environment early-init.el FILE with FORMS."
  (elpaca--write-file (expand-file-name "./early-init.el")
    (if file (insert-file-contents (expand-file-name file))
      (emacs-lisp-mode)
      (dolist (form forms) (prin1 form))
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
            "--eval" "(run-hooks 'emacs-startup-hook)"
            "--eval" "(message \"\n Test Env\n\")"
            "--eval" "(elpaca-version 'message)")
        `(,(format "--init-directory=%s" default-directory)))))

(defun elpaca-test--make-process (name buffer command vars)
  "Return subprocess with NAME BUFFER VARS executing COMMAND."
  (process-put (make-process :name name :buffer buffer :command command
                             :sentinel #'elpaca-test--sentinel)
               :vars vars))

(defun elpaca-test--announce (localp &optional ref)
  "Print test message for REF.
If LOCALP is non-nil use local repo information."
  (message
   "Testing Elpaca @ %s in %s"
   (if-let ((localp)
            (default-directory (expand-file-name "repos/elpaca/" elpaca-directory)))
       (concat (or (ignore-errors (elpaca-process-output "git" "diff" "--quiet")) "DIRTY ")
               (string-trim (elpaca-process-output "git" "log" "--pretty=%h %D" "-1")))
     (or ref "master"))
   default-directory))

;;;###autoload
(defmacro elpaca-test (&rest body)
  "Test Elpaca in a clean environment.
BODY is a plist which allows multiple values for certain keys.
The following keys are recognized:
  :name description of the test

  :ref git ref to check out or `local' to use local copy in current repo state

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
              Pending orders are failed after this time."
  (declare (indent 0))
  (unless lexical-binding (user-error "Lexical binding required for elpaca-test"))
  (let* ((args (elpaca-test--args body))
         (batchp (not (car (plist-get args :interactive))))
         (timeout (car (plist-get args :timeout)))
         (test (and batchp (elpaca-test--form args)))
         (init (plist-get args :init))
         (ref (car (plist-get args :ref)))
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
         (dir (elpaca-test--dir (car (plist-get args :dir))))
         print-length print-circle print-level
         eval-expression-print-level eval-expression-print-length)
    (elpaca-test--ensure-dir dir args)
    `(let* ((default-directory ,dir)
            (,procname (format "elpaca-test-%s" default-directory))
            (buffer ,@(if batchp `((generate-new-buffer ,procname)) '(nil))))
       (unless (file-exists-p default-directory) (make-directory default-directory 'parents))
       ,@(when localp '((elpaca-test--copy-local-store)))
       ,@(when early `((elpaca-test--write-early-init ,early-file ',(unless early-file early))))
       (elpaca--test-write-init ,init-file ',ref ',(when (or localp (null init-file))
                                                     (unless (equal init '(user)) init)))
       ,@(when-let ((before (plist-get args :before)))
           `((let ((default-directory default-directory)) ,@before)))
       (when buffer (elpaca-test--format-output-buffer buffer ,test))
       (elpaca-test--make-process
        ,procname buffer
        (elpaca-test--command ',(plist-get args :args) ,batchp ,timeout ',early)
        `(:computed-dir ,default-directory ,@',args))
       (elpaca-test--announce ,localp ,(unless localp ref)))))

(provide 'elpaca-test)
;;; elpaca-test.el ends here
