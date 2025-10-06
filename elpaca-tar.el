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
(defgroup elpaca-tar nil "Elpaca TAR Installation Support." :group 'elpaca :prefix "elpaca-tar-")
(defcustom elpaca-tar-url-templates
  '((github . "https://github.com/:user/:repo/tarball/:ref")
    (gitlab . "https://gitlab.com/:user/:repo/-/archive/:ref/:repo-:ref.tar.gz")
    (codeberg . "https://codeberg.org/:user/:repo/archive/:ref.tar.gz")
    (sourcehut . "https://git.sr.ht/~:user/:repo/archive/:ref.tar.gz")
    (gnu . "https://elpa.:host.org/packages/:package-:tar.tar")
    (nongnu . "https://elpa.:host.org/:host/:package-:tar.tar"))
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
                 (setq template (replace-regexp-in-string
                                 (symbol-name key) (format "%s" val) template nil t))
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
    (elpaca--signal e (format "Downloading and extracting tar to %s" src) 'extracting)
    (elpaca-with-emacs e
      (:args ("-L" (elpaca<-source-dir (elpaca-get 'elpaca))))
      (require 'elpaca-tar)
      (elpaca-tar--extract ,(elpaca-tar--url (elpaca<-recipe e)) ,src)
      (elpaca-tar--flatten ,src))))

(defcustom elpaca-tar-default-build-steps (list #'elpaca-tar-extract)
  "List of steps which are run when installing/building a package."
  :type '(repeat function))

(cl-defmethod elpaca-source ((e (elpaca tar)))
  "Download and extract E's :type `tar' source files."
  (setf (elpaca<-build-steps e)
        (append elpaca-tar-default-build-steps (elpaca<-build-steps e)))
  (elpaca--continue-build e))

(cl-defmethod elpaca-source-dir ((e (elpaca tar)))
  "Return source directory for E :type `tar`."
  (let ((recipe (elpaca<-recipe e)))
    (expand-file-name (or (cdr-safe (plist-get recipe :repo)) (plist-get recipe :package))
                      elpaca-sources-directory)))

(cl-defmethod elpaca--version ((e (elpaca tar)) &optional context)
  "Return version for :type `tar` E in CONTEXT."
  (cond ((eq context :alternative) (elpaca-ref e))))

(cl-defmethod elpaca-ref ((e (elpaca tar)))
  "Return :ref for :type tar E."
  (elpaca-with-dir e source
    (when-let* ((pkgdesc (expand-file-name (concat (elpaca<-package e) "-pkg.el")))
                (header (expand-file-name "pax_global_header"))
                (target (cond ((file-exists-p header) 'forge-tar)
                              ((file-exists-p pkgdesc) 'pkg))))
      (with-current-buffer (get-buffer-create " *elpaca-ref-parser*")
        (insert-file-contents-literally (if (eq target 'pkg) pkgdesc header) nil nil nil t)
        (goto-char (point-min))
        (if (eq target 'header)
            (when (re-search-forward "comment=\\(.*\\)" nil t) (match-string 1))
          (nth 2 (read (current-buffer))))))))

(provide 'elpaca-tar)
;;; elpaca-tar.el ends here
