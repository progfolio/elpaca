;;; elpaca-menu-elpa.el --- GNU/NonGNU ELPA recipe menu  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025 Nicholas Vollmer

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
(defvar url-http-response-status)

(defcustom elpaca-menu-elpa-use-mirror t
  "When non-nil utilize Github mirror of GNU/NonGNU Savannah ELPA."
  :type 'boolean :group 'elpaca)

(defvar elpaca-menu-elpas
  `((gnu . ((name         . "GNU ELPA")
            (cache . ,(elpaca--read-file (expand-file-name "gnu-elpa.eld" elpaca-cache-directory)))
            (cache-path   . ,(expand-file-name "gnu-elpa.eld" elpaca-cache-directory))
            (packages-url . ,(if elpaca-menu-elpa-use-mirror
                                 "https://raw.githubusercontent.com/emacsmirror/gnu_elpa/refs/heads/main/elpa-packages"
                               "https://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/elpa-packages"))
            (metadata-url . "https://elpa.gnu.org/packages/")
            (remote       . ,(if elpaca-menu-elpa-use-mirror "https://github.com/emacsmirror/gnu_elpa"
                               "https://git.savannah.gnu.org/git/emacs/elpa.git"))
            (branch-prefix . "externals")))
    (nongnu . ((name         . "NonGNU ELPA")
               (cache-path   . ,(expand-file-name "non-gnu-elpa.eld" elpaca-cache-directory))
               (cache . ,(elpaca--read-file (expand-file-name "non-gnu-elpa.eld" elpaca-cache-directory)))
               (packages-url . ,(if elpaca-menu-elpa-use-mirror
                                    "https://raw.githubusercontent.com/emacsmirror/nongnu_elpa/refs/heads/main/elpa-packages"
                                  "https://git.savannah.gnu.org/cgit/emacs/nongnu.git/plain/elpa-packages"))
               (metadata-url . "https://elpa.nongnu.org/nongnu/")
               (remote       . ,(if elpaca-menu-elpa-use-mirror
                                    "https://github.com/emacsmirror/nongnu_elpa"
                                  "https://git.savannah.gnu.org/git/emacs/nongnu.git"))
               (branch-prefix . "elpa")))))

(defun elpaca-menu-elpa--recipes (elpa)
  "Return list of recipes from ELPA."
  (let ((name (alist-get 'name elpa))
        (url (alist-get 'packages-url elpa)))
    (message "Downloading %s..." name)
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char url-http-end-of-headers)
      (condition-case err
          (progn (unless (equal url-http-response-status 200)
                   (error "%s responded with status %s" url url-http-response-status))
                 (read (current-buffer)))
        ((error) (always (lwarn `(elpaca menu) :warning
                                "Unable to read %s package file: %S" name err)))))))

(declare-function dom-by-tag "dom")
(declare-function dom-texts  "dom")
(defun elpaca-menu-elpa--metadata (elpa)
  "Return alist of ELPA package metadata."
  (when (libxml-available-p)
    (require 'dom)
    (with-current-buffer (url-retrieve-synchronously (alist-get 'metadata-url elpa) t)
      (when-let* ((html (libxml-parse-html-region (point-min) (point-max)))
                  (rows (dom-by-tag html 'tr)))
        (pop rows) ;discard table headers
        (mapcar (lambda (row)
                  (let* ((s (split-string (dom-texts row) " " 'omit-nulls))
                         (item (intern (pop s))))
                    (pop s) ; Discard version info here and "Rank" column below
                    (cons item (string-join (butlast s) " "))))
                rows)))))

(defcustom elpaca-menu-elpa-ignored-url-regexp "\\(?:bzr::\\|hg::\\|dr-qubit\\)"
  "Regular expression to ignore matching :url values." :type 'string :group 'elpaca)

(defcustom elpaca-menu-elpa-emacs-url "https://github.com/emacs-mirror/emacs"
  "URL to use for cloning Emacs for core packages." :type 'string :group 'elpaca)

(defun elpaca-menu-elpa--index (elpa)
  "Return ELPA TYPE menu item candidate list."
  (cl-loop
   with metadata      = (alist-get 'metadata-cache elpa)
   with elpa-name     = (alist-get 'name elpa)
   ;;with devel-name    = (replace-regexp-in-string " \\(.*\\)" "-devel \\1" elpa-name t)
   with remote        = (alist-get 'remote elpa)
   with metadata-url  = (alist-get 'metadata-url elpa)
   with branch-prefix = (alist-get 'branch-prefix elpa)
   with recipes       = (elpaca-menu-elpa--recipes elpa)
   initially (when (eq recipes t) (cl-return t))
   for (id . props) in recipes
   for core = (plist-get props :core)
   for releasep = (plist-get props :release-branch)
   when core do
   (setq core (mapcar (lambda (f) (if (equal f (file-name-as-directory f)) (concat f "*") f))
                      (if (listp core) core (list core))))
   for item =
   (when-let*
       ((name (symbol-name id))
        (url (or (and core elpaca-menu-elpa-emacs-url)
                 (and (plist-get props :manual-sync) remote) ;; developed in ELPA repo
                 (let ((declared (or (plist-get props :url) remote)))
                   (when (symbolp declared) ;; "sub-package" according to ELPA spec
                     (setq declared (or (plist-get (alist-get declared recipes) :url) remote)))
                   (if (string-match-p elpaca-menu-elpa-ignored-url-regexp declared) remote declared))))
        (recipe `( :package ,name :repo (,url . ,name)
                   ,@(or (and (or core (and url remote (eq url remote)))
                              `(:branch
                                ,(if core "master" (concat branch-prefix (when releasep "-release") "/" name))))
                         (when-let* ((branch (plist-get props :branch))) `(:branch ,branch)))
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
  (when-let* ((cache (alist-get 'cache elpa))
              ((not (eq cache t))))
    (elpaca--write-file (alist-get 'cache-path elpa) (prin1 cache))))

(defun elpaca-menu--elpa (name request &optional item)
  "Delegate REQUEST for NAME in `elpaca-menu-elpas'.
If REQUEST is `index`, return a recipe candidate alist.
If REQUEST is `update`, update the NonGNU ELPA recipe cache."
  (let ((elpa (alist-get name elpaca-menu-elpas)))
    (cond ((eq request 'index)
           (let ((cache (or (alist-get 'cache elpa)
                            (prog1
                                (setf (alist-get 'metadata-cache (alist-get name elpaca-menu-elpas))
                                      (elpaca-menu-elpa--metadata elpa)
                                      (alist-get 'cache (alist-get name elpaca-menu-elpas))
                                      (elpaca-menu-elpa--index (alist-get name elpaca-menu-elpas)))
                              (elpaca-menu-elpa--write-cache (alist-get name elpaca-menu-elpas))
                              (message "Downloading %s...100%%" (alist-get 'name elpa))))))
             (unless (eq cache t) (if item (elpaca-alist-get item cache) cache))))
          ((eq request 'update)
           (setf (alist-get 'cache (alist-get name elpaca-menu-elpas)) nil
                 (alist-get 'metadata-cache (alist-get name elpaca-menu-elpas)) nil)
           (elpaca-menu--elpa name 'index item)))))

;;;###autoload
(defun elpaca-menu-gnu-elpa (request &optional item)
  "Fulfill GNU ELPA menu `index` or `update` ITEM REQUEST."
  (elpaca-menu--elpa 'gnu request item))
;;;###autoload
(defun elpaca-menu-non-gnu-elpa (request &optional item)
  "Fulfill menu NonGNU ELPA `index` or `update` ITEM REQUEST."
  (elpaca-menu--elpa 'nongnu request item))

(provide 'elpaca-menu-elpa)
;;; elpaca-menu-elpa.el ends here
