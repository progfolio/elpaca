;;; elpaca-local.el --- Elpaca support for installing local files, directories  -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author:  Nicholas Vollmer
;; Keywords: files

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
(defgroup elpaca-local nil "Elpaca Local File Support." :group 'elpaca :prefix "elpaca-local-")
(defcustom elpaca-local-file-default-build-steps `(elpaca-local--set-src-dir ,@elpaca-build-steps)
  "List of steps which are run when installing/building a package."
  :type '(repeat function))

(defun elpaca-local--set-src-dir (e)
  "Set E's src directory."
  (setf (elpaca<-source-dir e) (file-name-directory (elpaca--main-file e)))
  (elpaca--continue-build e))

;;;###autoload
(defun elpaca-local-file-build-steps (e)
  "Return a contextual list of build steps if E is :type file."
  (when (equal (plist-get (elpaca<-recipe e) :type) 'file)
    (cond ((or (not after-init-time) (eq this-command 'elpaca))
           (if (file-exists-p (elpaca<-build-dir e))
               elpaca--pre-built-steps
             elpaca-local-file-default-build-steps))
          ((eq this-command 'elpaca-rebuild)
           (when (eq (elpaca--status e) 'finished)
             ;;@MAYBE: remove Info/load-path entries?
             (setf (elpaca<-build-steps e) elpaca-build-steps)))
          ((eq this-command 'elpaca-delete)
           (user-error "Refusing to delete local file %s" (elpaca--main-file e)))
          (t (elpaca--fail e (format "%S command not implemented" this-command))))))

(provide 'elpaca-local)
;;; elpaca-local.el ends here
