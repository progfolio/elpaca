;;; elpaca-menu-non-gnu-elpa.el --- NonGNU ELPA recipe menu  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Nicholas Vollmer

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
(require 'elpaca)
(require 'url)
(defvar url-http-end-of-headers)

(defvar elpaca-menu-non-gnu-elpa-cache-path
  (expand-file-name "non-gnu-elpa.eld" elpaca-cache-directory)
  "File name for NonGNU ELPA recipe cache.")

(defvar elpaca-menu-non-gnu-elpa-url
  "https://git.savannah.gnu.org/cgit/emacs/nongnu.git/plain/elpa-packages"
  "URL for NonGNU ELPA package list.")

(defvar elpaca-menu-non-gnu-elpa--index-cache
  (elpaca--read-file elpaca-menu-non-gnu-elpa-cache-path) "NonGNU ELPA recipe cache.")

(defun elpaca-menu-non-gnu-elpa--recipes ()
  "Return list of recipes from `elpaca-menu-non-gnu-elpa-url'."
  (message "Downloading NonGNU ELPA recipes...")
  (with-current-buffer (url-retrieve-synchronously elpaca-menu-non-gnu-elpa-url)
    (goto-char url-http-end-of-headers)
    (condition-case err
        (prog1 (read (current-buffer)) (message "NonGNU ELPA recipes downloaded"))
      ((error) (error "Unable to read NonGNU ELPA package file: %S" err)))))

(declare-function dom-by-tag "dom")
(declare-function dom-texts  "dom")
(defun elpaca-menu-non-gnu-elpa--metadata ()
  "Return alist of package metadata."
  (when (libxml-available-p)
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

(defun elpaca-menu-non-gnu-elpa--index (&optional recache)
  "Return candidate list of available NonGNU ELPA recipes.
If RECACHE is non-nil, recompute the cache."
  (or (and (not recache) elpaca-menu-non-gnu-elpa--index-cache)
      (prog1
          (setq elpaca-menu-non-gnu-elpa--index-cache
                (cl-loop with metadata = (elpaca-menu-non-gnu-elpa--metadata)
                         for (id . props) in (elpaca-menu-non-gnu-elpa--recipes)
                         for url = (plist-get props :url)
                         collect (list id
                                       :source "NonGNU ELPA"
                                       :url url
                                       :description (or (alist-get id metadata) "n/a")
                                       :recipe `( :package ,(symbol-name id) :repo ,url :url ,url
                                                  ,@(when-let ((ignored (plist-get props :ignored-files)))
                                                      (unless (listp ignored) (setq ignored (list ignored)))
                                                      `(:files (:defaults (:exclude ,@ignored))))))))
        (unless (file-exists-p elpaca-cache-directory) (make-directory elpaca-cache-directory))
        (elpaca--write-file elpaca-menu-non-gnu-elpa-cache-path elpaca-menu-non-gnu-elpa--index-cache))))

;;;###autoload
(defun elpaca-menu-non-gnu-elpa (request)
  "Delegate REQUEST.
If REQUEST is `index`, return a recipe candidate alist.
If REQUEST is `update`, update the NonGNU ELPA recipe cache."
  (elpaca-menu-non-gnu-elpa--index (eq request 'update)))

(provide 'elpaca-menu-non-gnu-elpa)
;;; elpaca-menu-non-gnu-elpa.el ends here
