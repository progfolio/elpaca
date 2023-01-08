;;; elpaca-process.el -- Functions for calling processes  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nicholas Vollmer

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

;; Functions for calling processes.

;;; Code:
(eval-when-compile (require 'subr-x))

(defvar elpaca-process-newline-regexp "[
]"
  "Regexp matching return or newline in process output.")

(defconst elpaca-process--stderr
  (expand-file-name (format "elpaca-stderr-%s" (emacs-pid))
                    temporary-file-directory)
  "File for storing proccesses' stderr.")

(defun elpaca--delete-stderr-file ()
  "Remove `elpaca-process--stderr' file."
  (when (and (boundp 'elpaca-process--stderr)
             (file-exists-p elpaca-process--stderr))
    (delete-file elpaca-process--stderr)))

(add-hook 'kill-emacs-hook #'elpaca--delete-stderr-file)

(defun elpaca-process-call (program &rest args)
  "Run PROGRAM syncrhonously with ARGS.
Return a list of form: (EXITCODE STDOUT STDERR).
If the process is unable to start, return an elisp error object."
  (let* ((program (if (string-match-p "/" program) (expand-file-name program) program)))
    (condition-case err
        (with-temp-buffer
          (list (apply #'call-process program nil
                       (list t elpaca-process--stderr)
                       nil args)
                (when-let ((s (buffer-substring-no-properties (point-min) (point-max)))
                           ((not (= 0 (length s)))))
                  s)
                (with-current-buffer (find-file-noselect elpaca-process--stderr
                                                         'nowarn 'raw)
                  (when-let ((s (buffer-substring-no-properties (point-min) (point-max)))
                             ((kill-buffer))
                             ((not (= 0 (length s)))))
                    s))))
      (error err))))

(declare-function elpaca--emacs-path "elpaca")
(defun elpaca-process-poll--filter (process output &optional pattern error)
  "Filter PROCESS OUTPUT.
PATTERN is a string which is checked against the entire process output.
If it matches, singal ERROR if non-nil."
  (process-put process :raw-output (concat (process-get process :raw-output) output))
  (unless (process-get process :messaged)
    (message "$%s" (string-join (process-command process) " "))
    (process-put process :messaged t))
  (let* ((result  (process-get process :result))
         (chunk   (concat result output))
         (lines   (split-string chunk "\n"))
         (linep   (= 0 (length (car (last lines))))))
    (unless linep
      (process-put process :result (car (last lines)))
      (setq lines (butlast lines)))
    (dolist (line lines)
      (unless (= 0 (length line)) (message "%s" line)))
    (when (and pattern error (string-match-p pattern output))
      (process-put process :result nil)
      (error "Subprocess filter error: %S" error))))

(defun elpaca-process-poll (program &rest args)
  "Run PROGRAM with ARGS aysnchronously, polling for messages.
This allows for output to be passed back to the parent Emacs process."
  (let* ((program (if (string-match-p "/" program) (expand-file-name program) program))
         (subprocess
          `(with-temp-buffer
             (setq load-prefer-newer t)
             (let ((p (make-process
                       :name   ,(concat "elpaca-process-poll-" program)
                       :buffer (current-buffer)
                       :command ',(cons program args))))
               (add-hook
                'after-change-functions
                (lambda (beg end _)
                  (when (process-live-p p)
                    (message "%s" (string-trim (buffer-substring-no-properties beg end)))))
                nil t)
               (while (accept-process-output p)))))
         (process (make-process
                   :name   (concat "elpaca-process-poll-" program)
                   :buffer (concat "elpaca-process-poll-" program)
                   :connecton-type 'pipe
                   :command (list (elpaca--emacs-path) "-Q" "--batch" "--eval"
                                  (format "%S" subprocess))
                   :filter #'elpaca-process-poll--filter)))
    (while (accept-process-output process))))

(defmacro elpaca-with-process (result &rest body)
  "Provide anaphoric RESULT bindings for duration of BODY.
RESULT must be an expression which evaluates to a list of form:
  (EXITCODE STDOUT STDERR)
Anaphroic bindings provided:
  result: the raw process result list
  exit: the exit code of the process
  invoked: t if process executed without an elisp error
  success: t if process exited with exit code 0
  failure: t if process did not invoke or exited with a nonzero code
  stdout: output of stdout
  stderr: output of stderr"
  (declare (indent 1) (debug t))
  `(let* ((result ,result)
          (exit (car result))
          (invoked (numberp exit))
          (success (and invoked (zerop exit)))
          (failure (not success))
          (stdout (nth 1 result))
          (stderr (nth 2 result)))
     ;; Stop the byte-compiler from complaining about unused bindings.
     (ignore result exit invoked success failure stdout stderr)
     ,@body))

(defun elpaca-process-output (program &rest args)
  "Return result of running PROGRAM with ARGS.
If the command cannot be run or returns a nonzero exit code, throw an error."
  (elpaca-with-process
      (apply #'elpaca-process-call program args)
    (cond
     (success       (concat stdout stderr)) ; Programs may exit normally and print to stderr
     ((not invoked) (error "%S" result))
     (t             (error "%s exited with code %s: %s" program (car result) stderr)))))

(provide 'elpaca-process)
;;; elpaca-process.el ends here
