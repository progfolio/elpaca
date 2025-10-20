;;; elpaca-tar.el --- Elpaca tarball support         -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025 Nicholas Vollmer

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

;;

;;; Code:
(require 'elpaca)
(require 'tar-mode)
(defgroup elpaca-git nil "Elpaca Git Repo Support." :group 'elpaca :prefix "elpaca-tar-")

(defcustom elpaca-tar-url-templates
  '((github .    "https://github.com/:user/:repo/tarball/:ref")
    (gitlab .    "https://gitlab.com/:user/:repo/-/archive/:ref/:repo-:ref.tar.gz")
    (codeberg .  "https://codeberg.org/:user/:repo/archive/:ref.tar.gz")
    (sourcehut . "https://git.sr.ht/~:user/:repo/archive/:ref.tar.gz"))
  "Alist of form ((HOST . URL-TEMPLATE)...)."
  :type 'alist)

(defun elpaca-tar--url (recipe)
  "Return URL from RECIPE."
  (or (plist-get recipe :url)
      (when-let* ((host (or (plist-get recipe :host) (plist-get recipe :fetcher)))
                  (repo (plist-get recipe :repo))
                  (template (alist-get host elpaca-tar-url-templates)))
        (when (consp repo) (setq repo (car repo)))
        (setq recipe (elpaca-merge-plists
                      recipe (list :repo
                                   (substring repo (- (string-match-p "/" (reverse repo))))
                                   :user
                                   (substring repo 0 (string-match-p "/" repo))
                                   :ref (or
                                         (plist-get recipe :ref)
                                         (plist-get recipe :branch)
                                         (plist-get recipe :tag)
                                         "HEAD"))))
        (cl-loop for (key val) on recipe by #'cddr do
                 (setq template (replace-regexp-in-string (symbol-name key) val template))
                 finally return template))
      (error "Unable to determine URL from recipe %s" recipe)))

(defvar url-http-response-status)
(defvar url-http-end-of-headers)
(defvar zlib-decompress-method)
(defun elpaca-tar--extract (url directory)
  "Extract tarball URL to DIRECTORY."
  (with-current-buffer (url-retrieve-synchronously url)
    (unless (equal url-http-response-status 200)
      (error "Unable to download %S %S" url url-http-response-status))
    (let ((default-directory directory)
          (zlib-decompress-method 'gzip))
      (unless (file-exists-p default-directory) (make-directory default-directory))
      (delete-region (point-min) (1+ (goto-char url-http-end-of-headers)))
      (zlib-decompress-region (point-min) (point-max))
      (tar-mode)
      (tar-untar-buffer))))

(defun elpaca-tar--set-src-dir (e)
  "Set E's source directory."
  (let ((recipe (elpaca<-recipe e)))
    (setf (elpaca<-source-dir e)
          (expand-file-name (or (cdr-safe (plist-get recipe :repo)) (plist-get recipe :package))
                            elpaca-repos-directory)))
  (elpaca--continue-build e))

(defun elpaca-tar--flatten (dir)
  "Move files in DIR's subdir to top-level of DIR.
Remove subdir."
  (let* ((default-directory dir)
         (dir (or (cl-some (lambda (it) (when (file-directory-p it) it))
                           (nthcdr 2 (directory-files default-directory)))
                  (error "Unable to determine extracted dir"))))
    (cl-loop for file in (nthcdr 2 (directory-files dir 'full))
             with dest = (file-name-as-directory default-directory)
             do (message "Moving %s to %s" file dest)
             (rename-file file dest)
             finally do (message "Deleting extraction directory %s" dir)
             (delete-directory dir))))

(defun elpaca-tar-extract (e)
  "Extract E's tarball."
  (let ((src (elpaca<-source-dir e)))
    (elpaca--signal e (format "Downloading and extracting tar to %s" src))
    (elpaca-with-emacs e
      (:args ("-L" (elpaca<-source-dir (elpaca-get 'elpaca))))
      (require 'elpaca-tar)
      (elpaca-tar--extract ,(elpaca-tar--url (elpaca<-recipe e)) ,src)
      (elpaca-tar--flatten ,src))))

(defcustom elpaca-tar-default-build-steps `(elpaca-tar--set-src-dir elpaca-tar-extract ,@elpaca-build-steps)
  "List of steps which are run when installing/building a package."
  :type '(repeat function))

(cl-defmethod elpaca-build-steps ((e (elpaca tar)) &optional context)
  "Return :type tar E's build steps for CONTEXT."
  (pcase context
    ('nil `(:first ,@(if (elpaca<-builtp e) (list 'elpaca-tar--set-src-dir)
                       elpaca-tar-default-build-steps)))))

(cl-defmethod elpaca--version ((e (elpaca tar)) &optional context)
  "Return :type tar E's version given CONTEXT."
  (cond ((eq context :alternative) (elpaca-ref e))))

(cl-defmethod elpaca-ref ((e (elpaca tar)))
  "Return :type tar E's ref"
  (elpaca-with-dir e source
    (when-let* ((header (expand-file-name "pax_global_header"))
                (file-exists-p header))
      (with-temp-buffer
        (insert-file-contents-literally header)
        (when (re-search-forward "comment=\\(.*\\)" nil t)
          (match-string 1))))))

(provide 'elpaca-tar)
;;; elpaca-tar.el ends here
