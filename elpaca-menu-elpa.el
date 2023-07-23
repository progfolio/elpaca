;;; elpaca-menu-elpa.el --- GNU/NonGNU ELPA recipe menu  -*- lexical-binding: t; -*-

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
(defvar elpaca-menu-elpas
  (cl-loop
   for (id . props) in
   `((gnu . ((name         . "GNU ELPA")
             (cache-path   . ,(expand-file-name "gnu-elpa.eld" elpaca-cache-directory))
             (packages-url . "https://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/elpa-packages")
             (metadata-url . "https://elpa.gnu.org/packages/")
             (remote       . "git://git.sv.gnu.org/emacs/elpa")
             (branch-prefix . "externals")))
     (nongnu . ((name         . "NonGNU ELPA")
                (cache-path   . ,(expand-file-name "non-gnu-elpa.eld" elpaca-cache-directory))
                (packages-url . "https://git.savannah.gnu.org/cgit/emacs/nongnu.git/plain/elpa-packages")
                (metadata-url . "https://elpa.nongnu.org/nongnu/")
                (remote       . "git://git.sv.gnu.org/emacs/nongnu")
                (branch-prefix . "elpa"))))
   for dev = (copy-tree props)
   do (setf (alist-get 'name dev) (replace-regexp-in-string " "  "-devel " (alist-get 'name dev))
            (alist-get 'devel dev) t
            (alist-get 'cache-path dev)
            (replace-regexp-in-string "\\([^z-a]+-\\)" "\\1devel-" (alist-get 'cache-path dev))
            (alist-get 'cache dev) (elpaca--read-file (alist-get 'cache-path dev))
            (alist-get 'cache props) (elpaca--read-file (alist-get 'cache-path props)))
   collect (cons id props)
   collect (cons (intern (concat (symbol-name id) "-devel")) dev)))

(defun elpaca-menu-elpa--recipes (elpa)
  "Return list of recipes from ELPA."
  (let ((name (alist-get 'name elpa)))
    (message "Downloading %s..." name)
    (with-current-buffer (url-retrieve-synchronously (alist-get 'packages-url elpa) t)
      (goto-char url-http-end-of-headers)
      (condition-case err
          (read (current-buffer))
        ((error) (error "Unable to read %s package file: %S" name err))))))

(declare-function dom-by-tag "dom")
(declare-function dom-texts  "dom")
(defun elpaca-menu-elpa--metadata (elpa)
  "Return alist of ELPA package metadata."
  (or (alist-get 'metadata-cache elpa)
      (when (libxml-available-p)
        (require 'dom)
        (with-current-buffer (url-retrieve-synchronously (alist-get 'metadata-url elpa) t)
          (when-let ((html (libxml-parse-html-region (point-min) (point-max)))
                     (rows (dom-by-tag html 'tr)))
            (pop rows) ;discard table headers
            (setf (alist-get 'metadata-cache elpa)
                  (mapcar (lambda (row)
                            (let* ((s (split-string (dom-texts row) " " 'omit-nulls))
                                   (item (intern (pop s))))
                              (pop s) ;discard version info
                              (cons item (string-join s " "))))
                          rows)))))))

(defcustom elpaca-menu-elpa-ignored-url-regexp "\\(?:bzr::\\|hg::\\|dr-qubit\\)"
  "Regular expression to ignore matching :url values." :type 'string :group 'elpaca)

(defcustom elpaca-menu-elpa-emacs-url "https://github.com/emacs-mirror/emacs"
  "URL to use for cloning Emacs for core packages." :type 'string :group 'elpaca)

(defun elpaca-menu-elpa--index (elpa)
  "Return ELPA TYPE menu item candidate list."
  (cl-loop
   with releasep      = (not (alist-get 'devel elpa))
   with metadata      = (elpaca-menu-elpa--metadata elpa)
   with elpa-name     = (alist-get 'name elpa)
   with remote        = (alist-get 'remote elpa)
   with metadata-url  = (alist-get 'metadata-url elpa)
   with branch-prefix = (alist-get 'branch-prefix elpa)
   for (id . props) in (elpaca-menu-elpa--recipes elpa)
   for core = (plist-get props :core)
   when core do
   (setq core (mapcar (lambda (f) (if (equal f (file-name-as-directory f)) (concat f "*") f))
                      (if (listp core) core (list core))))
   for item =
   (when-let
       (((or core (if releasep (plist-get props :release-branch) t)))
        (name (symbol-name id))
        (url (or (and core elpaca-menu-elpa-emacs-url)
                 (if-let ((declared (plist-get props :url))
                          ;;Why does ELPA keep the :url when upstream is gone?
                          ((not (or releasep (string-match-p elpaca-menu-elpa-ignored-url-regexp declared)))))
                     declared
                   remote)))
        (recipe `( :package ,name :repo ,url :local-repo ,name
                   ,@(or (and (or core (eq url remote))
                              `(:branch
                                ,(if core "master" (concat branch-prefix (when releasep "-release") "/" name))))
                         (when-let ((branch (plist-get props :branch))) `(:branch ,branch)))
                   ,@(let ((ignored (plist-get props :ignored-files)))
                       ;;@TODO: support :core ((file new-name)...) format
                       `(:files (,@(or core '("*")) ; ("*" :exclude ".git") is what package/straight.el default to.
                                 ,@(list (append '(:exclude ".git") (if (listp ignored) ignored (list ignored)))))))))
        (item-props (list :source elpa-name
                          :url (concat metadata-url name ".html")
                          :description (or (alist-get id metadata) "n/a")
                          :recipe recipe)))
     (cons id item-props))
   when item collect item))

(defun elpaca-menu-elpa--write-cache (elpa)
  "Write ELPA menu item cache."
  (unless (file-exists-p elpaca-cache-directory) (make-directory elpaca-cache-directory))
  (elpaca--write-file (alist-get 'cache-path elpa) (prin1 (alist-get 'cache elpa))))

(defun elpaca-menu--elpa (elpa request &optional recurse)
  "Delegate REQUEST for ELPA.
If REQUEST is `index`, return a recipe candidate alist.
If REQUEST is `update`, update the NonGNU ELPA recipe cache.
If RECURSE is non-nil, message that update succeeded."
  (cond ((eq request 'index)
         (or (alist-get 'cache elpa)
             (prog1
                 (setf (alist-get 'cache elpa) (elpaca-menu-elpa--index elpa))
               (elpaca-menu-elpa--write-cache elpa)
               (when recurse (message "Downloading %s...100%%" (alist-get 'name elpa))))))
        ((eq request 'update)
         (delete-file (alist-get 'cache-path elpa))
         (setf (alist-get 'cache elpa) nil (alist-get 'metadata-cache elpa) nil)
         (elpaca-menu--elpa elpa 'index 'recurse))))

;;;###autoload
(defun elpaca-menu-gnu-elpa (request)
  "Fulfill GNU ELPA menu `index` or `update` REQUEST."
  (elpaca-menu--elpa (alist-get 'gnu elpaca-menu-elpas) request))
;;;###autoload
(defun elpaca-menu-gnu-devel-elpa (request)
  "Fulfill GNU ELPA-devel menu `index` or `update` REQUEST."
  (elpaca-menu--elpa (alist-get 'gnu-devel elpaca-menu-elpas) request))
;;;###autoload
(defun elpaca-menu-non-gnu-elpa (request)
  "Fulfill menu NonGNU ELPA `index` or `update` REQUEST."
  (elpaca-menu--elpa (alist-get 'nongnu elpaca-menu-elpas) request))
;;;###autoload
(defun elpaca-menu-non-gnu-devel-elpa (request)
  "Fulfill menu NonGNU ELPA-devel `index` or `update` REQUEST."
  (elpaca-menu--elpa (alist-get 'nongnu-devel elpaca-menu-elpas) request))

(provide 'elpaca-menu-elpa)
;;; elpaca-menu-elpa.el ends here
