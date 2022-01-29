;;; parcel-process.el -- Functions for calling processes  -*- lexical-binding: t; -*-

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

(defconst parcel-process--stderr
  (expand-file-name (format "parcel-stderr-%s" (emacs-pid))
                    temporary-file-directory)
  "File for storing proccesses' stderr.")

(defun parcel-process-call (program &rest args)
  "Run PROGRAM syncrhonously with ARGS.
Return a list of form: (EXITCODE STDOUT STDERR).
If the process is unable to start, return an elisp error object."
  (let* ((program (if (string-match-p "/" program) (expand-file-name program) program)))
    (condition-case err
        (with-temp-buffer
          (list (apply #'call-process program nil
                       (list (current-buffer) parcel-process--stderr)
                       nil args)
                (let ((s (buffer-string))) (unless (string-empty-p s) s))
                (with-current-buffer (find-file-noselect parcel-process--stderr
                                                         'nowarn 'raw)
                  (prog1 (let ((s (buffer-string))) (unless (string-empty-p s) s))
                    (kill-buffer)))))
      (error err))))

(defmacro parcel-with-process (result &rest body)
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

(defmacro parcel-with-async-process (process &rest body)
  "Execute BODY after PROCESS is run asynchronously."
  (declare (indent 1) (debug t))
  `(let ((result))
     (make-process
      :name "parcel-async"
      :filter (lambda (proc output) (setq result (cons result output)))
      :sentinel (lambda (proc event)
                  (when (equal event "finished\n")
                    (eval `(parcel-with-process ',(read (cdr result))
                             ,@',body)
                          t)))
      :command (list (concat invocation-directory invocation-name)
                     "-L" parcel-directory
                     "-l" (expand-file-name "parcel/parcel-process.el" parcel-directory)
                     "--batch"
                     "--eval"
                     ,(format "(message \"%%S\" (apply #'parcel-process-call '%S))" process)))))

(defun parcel-process-output (program &rest args)
  "Return result of running PROGRAM with ARGS.
If the command cannot be run or returns a nonzero exit code, throw an error."
  (parcel-with-process
      (apply #'parcel-process-call program args)
    (cond
     (success       (concat stdout stderr)) ; Programs may exit normally and print to stderr
     ((not invoked) (error "%S" result))
     (t             (error "%s exited with code %s: %s" program (car result) stderr)))))

(defvar parcel-process-newline-regexp "[
]"
  "Regexp matching return or newline in process output.")

(defun parcel-process-tail (string)
  "Return trimmed tail of process output STRING."
  (car (last (split-string string parcel-process-newline-regexp 'omit-nulls " +"))))

(provide 'parcel-process)
;;; parcel-process.el ends here
