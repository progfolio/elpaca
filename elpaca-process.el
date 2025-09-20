;;; elpaca-process.el -- Functions for calling processes  -*- lexical-binding: t; -*-

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

;; Functions for calling processes.

;;; Code:
(eval-when-compile (require 'subr-x))

(defvar elpaca-process-newline-regexp "[
]"
  "Regexp matching return or newline in process output.")

(defconst elpaca-process--stderr
  (expand-file-name (format "elpaca-stderr-%s" (emacs-pid)) temporary-file-directory)
  "File for storing processes' stderr.")

(defun elpaca--delete-stderr-file ()
  "Remove `elpaca-process--stderr' file."
  (when (and (boundp 'elpaca-process--stderr) (file-exists-p elpaca-process--stderr))
    (delete-file elpaca-process--stderr)))

(add-hook 'kill-emacs-hook #'elpaca--delete-stderr-file)

(defun elpaca-process-call (program &rest args)
  "Run PROGRAM synchronously with ARGS.
Return a list of form: (EXITCODE STDOUT STDERR).
If the process is unable to start, return an elisp error object."
  (when (string-match-p "/" program) (setq program (expand-file-name program)))
  (let ((dir default-directory))
    (with-current-buffer (get-buffer-create " elpaca-process-call")
      (erase-buffer)
      (setq default-directory dir)
      (list (apply #'call-process program nil (list t elpaca-process--stderr) nil args)
            (unless (= (buffer-size) 0) (buffer-substring-no-properties (point-min) (point-max)))
            (unless (= (file-attribute-size (file-attributes elpaca-process--stderr)) 0)
              (insert-file-contents elpaca-process--stderr nil nil nil t)
              (buffer-substring-no-properties (point-min) (point-max)))))))

(declare-function elpaca--emacs-path "elpaca")

(defmacro elpaca-with-process (result &rest body)
  "Provide anaphoric RESULT bindings for duration of BODY.
RESULT must be an expression which evaluates to a list of form:
  (EXITCODE STDOUT STDERR)
Anaphoric bindings provided:
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

(defmacro elpaca-with-process-call (args &rest body)
  "Evaluate BODY in `elpaca-with-process', applying `elpaca-process-call' to ARGS."
  (declare (indent 1) (debug 'form))
  `(elpaca-with-process (elpaca-process-call ,@(if (listp args) args (list args))) ,@body))

(defmacro elpaca-process-cond (args &rest conditions)
  "Eval CONDITIONS in context of `elpaca-with-process-call' with ARGS."
  (declare (indent 1) (debug t))
  `(elpaca-with-process-call ,args (cond ,@conditions)))

(defun elpaca-process-output (program &rest args)
  "Return result of running PROGRAM with ARGS.
If the command cannot be run or returns a nonzero exit code, throw an error."
  (elpaca-with-process (apply #'elpaca-process-call program args)
    (cond
     (success       (concat stdout stderr)) ; Programs may exit normally and print to stderr
     ((not invoked) (error "%S" result))
     (t             (error "%s exited with code %s: %s" program (car result) stderr)))))

(provide 'elpaca-process)
;;; elpaca-process.el ends here
