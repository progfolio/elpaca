;;; parcel-menu-non-gnu-elpa.el --- NonGNU ELPA recipe menu  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nicholas Vollme

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
(require 'cl-lib)
(require 'parcel)

(defcustom parcel-menu-non-gnu-elpa-cache-path
  (expand-file-name "cache/non-gnu-elpa.eld" parcel-directory)
  "File name for NonGNU ELPA recipe cache."
  :type 'file
  :group 'parcel)

(defvar parcel-menu-non-gnu-elpa-url
  "https://git.savannah.gnu.org/cgit/emacs/nongnu.git/plain/elpa-packages"
  "URL for NonGNU ELPA package list.")

(defvar parcel-menu-non-gnu-elpa--index-cache
  (when (file-exists-p parcel-menu-non-gnu-elpa-cache-path)
    (parcel--read-file parcel-menu-non-gnu-elpa-cache-path))
  "Cache of NonGNU ELPA recipes.")

(defvar url-http-end-of-headers)
(defun parcel-menu-non-gnu-elpa--recipes ()
  "Return list of recipes from `parcel-menu-non-gnu-elpa-url'."
  (with-current-buffer (url-retrieve-synchronously parcel-menu-non-gnu-elpa-url)
    (goto-char url-http-end-of-headers)
    (condition-case err
        (read (current-buffer))
      ((error) (error "Unable to read NonGNU ELPA package file: %S" err)))))

(declare-function dom-by-tag "dom")
(declare-function dom-texts  "dom")
(defun parcel-menu-non-gnu-elpa--metadata ()
  "Return alist of package metadata."
  (when (libxml-available-p)
    (require 'url)
    (require 'dom)
    (with-current-buffer (url-retrieve-synchronously "https://elpa.nongnu.org/nongnu/")
      (when-let ((html (libxml-parse-html-region (point-min) (point-max)))
                 (rows (dom-by-tag html 'tr)))
        (pop rows) ;discard table headers
        (mapcar (lambda (row)
                  (let* ((s (split-string (dom-texts row) " " 'omit-nulls))
                         (item (intern (pop s))))
                    (pop s) ;discard version info
                    (cons item (string-join s " "))))
                rows)))))

(defun parcel-menu-non-gnu-elpa--index ()
  "Return candidate list of available NonGNU ELPA recipes."
  (let ((metadata (parcel-menu-non-gnu-elpa--metadata)))
    (cl-loop for recipe in (parcel-menu-non-gnu-elpa--recipes)
             for name = (pop recipe)
             for item = (intern name)
             for url = (plist-get recipe :url)
             collect
             (list item
                   :source "NonGNU ELPA"
                   :url url
                   :description (or (alist-get item metadata) "n/a")
                   :recipe
                   `( :package ,name
                      :repo ,url
                      :url ,url
                      ,@(when-let ((ignored (plist-get recipe :ignored-files)))
                          `(:files (:defaults (:not ,@ignored)))))))))

(defun parcel-menu-non-gnu-elpa--write-cache ()
  "Write NonGNU ELPA menu item cache."
  (parcel--write-file parcel-menu-non-gnu-elpa-cache-path
    (prin1 (parcel-menu-non-gnu-elpa--index))))

;;;###autoload
(defun parcel-menu-non-gnu-elpa (request &optional recurse)
  "Delegate REQUEST.
If REQUEST is `index`, return a recipe candidate alist.
If REQUEST is `update`, update the NonGNU ELPA recipe cache.
If RECURSE is non-nil, message that update succeeded."
  (cond
   ((eq request 'index)
    (or parcel-menu-non-gnu-elpa--index-cache
        (progn
          (parcel-menu-non-gnu-elpa--write-cache)
          (prog1
              (setq parcel-menu-non-gnu-elpa--index-cache
                    (parcel-menu-non-gnu-elpa--index))
            (when recurse (message "NonGNU ELPA menu updated."))))))
   ((eq request 'update)
    (delete-file parcel-menu-non-gnu-elpa-cache-path)
    (setq parcel-menu-non-gnu-elpa--index-cache nil)
    (parcel-menu-non-gnu-elpa 'index))))

(provide 'parcel-menu-non-gnu-elpa)
;;; parcel-menu-non-gnu-elpa.el ends here
