;;; parcel-ideas.el --- ideas which may be incorporated into parcel at some point  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nicholas Vollmer

;; Author:  Nicholas Vollmer
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
(require 'parcel)
(defvar parcel-directory)
(defvar parcel--finalize-queue-hook)
(defvar parcel-status-auto-kill)
(defvar parcel-status-buffer)

(defmacro parcel-with-dir (type item &rest body)
  "Set `default-directory' for duration of BODY.
TYPE is either `:repo' or `:build' for ITEM's repo or build directory."
  (declare (indent 2) (debug t))
  `(let ((default-directory
          (,(intern (format "parcel-%s-dir" (substring (symbol-name type) 1)))
           (parcel-recipe ,item))))
     ,@body))


(defun parcel-package-file-p ()
  "Return t if current buffer's file is part of a repo."
  (interactive)
  (and-let* ((name (buffer-file-name))
             ((string-prefix-p (expand-file-name "./repos" parcel-directory) name))
             ((cl-find-if (lambda (q) (member name
                                              (mapcar #'car (parcel--files (cdr q)))))
                          (parcel--queued-orders))))))

(defun parcel-ui--post-maybe-rebuild ()
  (setq parcel--finalize-queue-hook nil)
  (when parcel-status-auto-kill (kill-buffer parcel-status-buffer)))

(defun parcel-maybe-rebuild-package ()
  "Rebuild package associated with BUFFER."
  (interactive)
  (when-let ((queued (parcel-package-file-p)))
    (parcel-split-queue)
    (setq parcel--finalize-queue-hook '(parcel-ui--post-maybe-rebuild))
    (parcel-rebuild-package (car queued))))

(provide 'parcel-ideas)
;;; parcel-ideas.el ends here
