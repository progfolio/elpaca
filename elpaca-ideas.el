;;; elpaca-ideas.el --- ideas which may be incorporated into elpaca at some point  -*- lexical-binding: t; -*-

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
(require 'elpaca)
(defvar elpaca-directory)
(defvar elpaca--finalize-queue-hook)
(defvar elpaca-status-auto-kill)
(defvar elpaca-log-buffer)

;;;###autoload
(defmacro elpaca-with-dir (type item &rest body)
  "Set `default-directory' for duration of BODY.
TYPE is either `:repo' or `:build' for ITEM's repo or build directory."
  (declare (indent 2) (debug t))
  `(let ((default-directory
          (,(intern (format "elpaca-%s-dir" (substring (symbol-name type) 1)))
           (elpaca-recipe ,item))))
     ,@body))

(provide 'elpaca-ideas)
;;; elpaca-ideas.el ends here
