;;; elpaca-menu-elpa.el --- GNU/NonGNU ELPA recipe menu  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2026 Nicholas Vollmer

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

;; GNU/NonGNU ELPA menu.
;;
;; `elpaca-menu-elpa-create NAME URL' defines a named menu function for
;; the archive and returns its symbol, for use in `elpaca-menu-functions'
;; or `:inherit'.  `elpaca-menu-elpa-gnu' and `elpaca-menu-elpa-nongnu'
;; are the built-in instances.

;;; Code:
(require 'elpaca)
(require 'json)
(require 'url)
(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defcustom elpaca-menu-elpa-use-mirror t
  "When non-nil use the GitHub mirror for the ELPA `elpa-packages' file."
  :type 'boolean
  :group 'elpaca)

(defun elpaca-menu-elpa--name (name)
  "Return NAME lower-cased with non-alphanumerics replaced by hyphens."
  (replace-regexp-in-string "[^a-z0-9-]" "-" (downcase name)))

(defun elpaca-menu-elpa-dotted-version (version)
  "Return VERSION list (e.g. `(2 20 0)') as a dotted string (\"2.20.0\")."
  (mapconcat #'number-to-string version "."))

(defun elpaca-menu-elpa--contents (root)
  "Return the parsed `archive-contents' of the archive rooted at ROOT.
The result is a list of (PKG . [VERSION DEPS DESCRIPTION TYPE METADATA]),
or `t` on fetch/parse failure."
  (let* ((url (concat (file-name-as-directory root) "archive-contents")))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char url-http-end-of-headers)
      (condition-case err
          (progn
            (unless (equal url-http-response-status 200)
              (signal 'http-error (list url url-http-response-status)))
            (let ((blob (read (current-buffer))))
              (unless (numberp (elt blob 0))
                (signal 'wrong-type-argument `(numberp ,(elt blob 0))))
              (cdr blob)))
        ((error) (message "elpaca-menu-elpa: unable to read %s: %S" url err) t)))))

(defun elpaca-menu-elpa--artifact-url (root pname version type)
  "Return ROOT artifact URL for package PNAME VERSION of TYPE."
  (concat (file-name-as-directory root) pname "-" (elpaca-menu-elpa-dotted-version version)
          (if (eq type 'tar) ".tar" ".el")))

(defun elpaca-menu-elpa--upstream (meta)
  "Return the upstream/project URL stored in META, or nil if not present.
META may be a list of (KEYWORD . VALUE) pairs, or a plist."
  (if-let* (((consp (car-safe meta)))
            (pair (or (assoc :url meta) (assoc :upstream meta))))
      (let ((val (cdr pair))) (if (listp val) (car val) val))
    (or (plist-get meta :url) (plist-get meta :upstream))))

(defun elpaca-menu-elpa--packages-url (name)
  "Return the `elpa-packages' URL for archive NAME (`gnu' or `nongnu')."
  (cond
   ((equal name "gnu")
    (if elpaca-menu-elpa-use-mirror
        "https://raw.githubusercontent.com/emacsmirror/gnu_elpa/refs/heads/main/elpa-packages"
      "https://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/elpa-packages"))
   ((equal name "nongnu")
    (if elpaca-menu-elpa-use-mirror
        "https://raw.githubusercontent.com/emacsmirror/nongnu_elpa/refs/heads/main/elpa-packages"
      "https://git.savannah.gnu.org/cgit/emacs/nongnu.git/plain/elpa-packages"))
   (nil)))

(defun elpaca-menu-elpa--repo-base-url (name)
  "Return the ELPA mirror repository URL for archive NAME."
  (cond
   ((equal name "gnu")
    (if elpaca-menu-elpa-use-mirror "https://github.com/emacsmirror/gnu_elpa"
      "https://git.savannah.gnu.org/git/emacs/elpa"))
   ((equal name "nongnu")
    (if elpaca-menu-elpa-use-mirror "https://github.com/emacsmirror/nongnu_elpa"
      "https://git.savannah.gnu.org/git/emacs/nongnu"))
   (nil)))

(defun elpaca-menu-elpa--branch (name pname)
  "Return the externals branch for archive NAME and package PNAME."
  (cond
   ((equal name "gnu") (concat "externals/" pname))
   ((equal name "nongnu") (concat "elpa/" pname))
   (nil)))

(defun elpaca-menu-elpa--packages (name)
  "Return NAME's `elpa-packages' list of form (NAME :url URL OPTS...).
Returns t on fetch/parse failure."
  (when-let* ((url (elpaca-menu-elpa--packages-url name)))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char url-http-end-of-headers)
      (condition-case err
          (progn
            (unless (equal url-http-response-status 200)
              (signal 'http-error (list url url-http-response-status)))
            (read (current-buffer)))
        ((error)
         (message "elpaca-menu-elpa: unable to read %s: %S" url err)
         t)))))

(defun elpaca-menu-elpa--item (pkg name root version desc type meta pkg-entry)
  "Build a menu item cell for package PKG from archive NAME rooted at ROOT.
VERSION is the version list, DESC the description string, TYPE the
package kind (`tar' or `single'), META the package metadata, and
PKG-ENTRY the `elpa-packages' entry (or nil). The `elpa-packages' keys
are merged into the recipe: :url → :repo (upstream); when :url is nil,
:repo is the ELPA mirror repo and :branch the externals branch."
  (let* ((pname (symbol-name pkg))
         (pkg-url (and pkg-entry (plist-get (cdr pkg-entry) :url)))
         (real-upstream (or pkg-url
                            (and (null pkg-entry) (elpaca-menu-elpa--upstream meta))))
         (browse-url (or real-upstream
                         (concat (file-name-as-directory root) pname ".html")))
         (repo (if real-upstream
                   real-upstream
                 (and pkg-entry (elpaca-menu-elpa--repo-base-url name))))
         (base (append (list :package pname)
                       (when repo `(:repo (,repo . ,pname)))
                       (and pkg-entry (null pkg-url)
                            (list :branch (elpaca-menu-elpa--branch name pname)))
                       (when (eq type 'tar)
                         (list :url (elpaca-menu-elpa--artifact-url root pname version 'tar)
                               :tar (elpaca-menu-elpa-dotted-version version)))
                       (when (eq type 'single)
                         (list :url (elpaca-menu-elpa--artifact-url root pname version 'single)))))
         (recipe (if pkg-entry
                     (elpaca-merge-plists base
                                         (cl-loop for (k v) on (cdr pkg-entry) by #'cddr
                                                  when (not (eq k :url))
                                                  append (list k v)))
                   base)))
    (cons pkg
          (list :source name
                :description (or desc "")
                :url browse-url
                :recipe recipe))))

(defun elpaca-menu-elpa--source (name root)
  "Return menu cells for archive NAME at ROOT, built from its archive-contents.
Returns nil on fetch failure."
  (let* ((contents (elpaca-menu-elpa--contents root))
         (packages (elpaca-menu-elpa--packages name)))
    (when (listp contents)
      (cl-loop for (pkg . v) in contents
               for entry = (and (listp packages) (assoc pkg packages))
               collect (elpaca-menu-elpa--item pkg name root
                                               (elt v 0) (elt v 2)
                                               (elt v 3) (elt v 4) entry)))))

(defun elpaca-menu-melpa--contents ()
  "Return the parsed MELPA `recipes.json' as a list of (PKG . RECIPE) pairs.
RECIPE is a list of (KEY . VALUE) pairs (symbol keys). Returns non-nil on
fetch/parse failure."
  (let ((url "https://melpa.org/recipes.json"))
    (with-current-buffer (url-retrieve-synchronously url t)
      (goto-char url-http-end-of-headers)
      (condition-case err
          (progn
            (unless (equal url-http-response-status 200)
              (signal 'http-error (list url url-http-response-status)))
            (json-read))
        ((error)
         (message "elpaca-menu-elpa: unable to read %s: %S" url err)
         t)))))

(defun elpaca-menu-melpa--item (pkg name root recipe desc)
  "Build a menu item cell with a git recipe for package PKG.
The cell is for archive NAME at ROOT. RECIPE is a list of (KEY . VALUE)
pairs from `recipes.json' (symbol keys). The recipe is passed through the
repo, fetcher, files, branch, and version-regexp keys; Elpaca installs the
repo's default branch. DESC is the package description from archive-contents,
or nil."
  (let* ((pname (symbol-name pkg))
         (repo (or (cdr (assoc 'repo recipe)) (cdr (assoc 'url recipe))))
         (fetcher (cdr (assoc 'fetcher recipe)))
         (files (cdr (assoc 'files recipe)))
         (branch (cdr (assoc 'branch recipe)))
         (version-regexp (cdr (assoc 'version-regexp recipe)))
         (browse (or (cdr (assoc 'url recipe))
                     (concat (file-name-as-directory root) pname ".html")))
         (r (append (list :package pname)
                    (when repo `(:repo ,repo))
                    (when fetcher `(:fetcher ,(intern (downcase fetcher))))
                    (when files `(:files ,(read files)))
                    (when branch `(:branch ,branch))
                    (when version-regexp `(:version-regexp ,version-regexp)))))
    (cons pkg
          (list :source name
                :description (or desc "")
                :url browse
                :recipe r))))

(defun elpaca-menu-melpa--source (name root)
  "Return menu cells for archive NAME (MELPA) at ROOT.
Built from `recipes.json' (recipe) and archive-contents (description).
Returns nil on fetch failure."
  (let* ((data (elpaca-menu-melpa--contents))
         (contents (elpaca-menu-elpa--contents root))
         (descs (and (listp contents)
                     (cl-loop for (pkg . v) in contents
                              collect (cons pkg (elt v 2))))))
    (when (listp data)
      (cl-loop for (pkg . recipe) in data
               for desc = (cdr (assoc pkg descs))
               collect (elpaca-menu-melpa--item pkg name root recipe desc)))))

(defun elpaca-menu-elpa--cache-path (name)
  "Return the cache file path for archive NAME."
  (expand-file-name (concat "elpaca-menu-"
                            (elpaca-menu-elpa--name name)
                            ".eld")
                    elpaca-cache-directory))

(defun elpaca-menu-elpa--write-cache (name cache)
  "Write CACHE for archive NAME to its cache file."
  (unless (file-exists-p elpaca-cache-directory)
    (make-directory elpaca-cache-directory 'parents))
  (elpaca--write-file (elpaca-menu-elpa--cache-path name)
    (prin1 cache)))

(defun elpaca-menu-elpa--index (name root source-fn &optional item)
  "Return menu items for archive NAME at ROOT, reading from / writing its cache.
SOURCE-FN builds the cells (fetching the data as needed). If ITEM is
non-nil return only that item (or nil if not found)."
  (let ((cache (elpaca--read-file (elpaca-menu-elpa--cache-path name))))
    (unless cache
      (when-let* ((cells (funcall source-fn name root)))
        (elpaca-menu-elpa--write-cache name cells)
        (setq cache cells)))
    (if item (elpaca-alist-get item cache) cache)))

(defun elpaca-menu-elpa--update (name root source-fn)
  "Update the cache for archive NAME at ROOT using SOURCE-FN; return the new index."
  (let ((path (elpaca-menu-elpa--cache-path name)))
    (when (file-exists-p path) (delete-file path))
    (elpaca-menu-elpa--index name root source-fn)))

(defun elpaca-menu-elpa--run (name url request source-fn &optional item)
  "Fulfill menu REQUEST for archive NAME at URL.
REQUEST is `index' (return items) or `update' (rebuild cache and return items).
SOURCE-FN builds the cells."
  (pcase request
    ('index  (elpaca-menu-elpa--index name url source-fn item))
    ('update (elpaca-menu-elpa--update name url source-fn)
             (elpaca-menu-elpa--index name url source-fn item))))

;;;###autoload
(defun elpaca-menu-elpa-create (name url &optional sym source-fn)
  "Return a symbol for a named menu function defined for NAME at URL.
The function is defined under SYM if non-nil, else under a derived
`elpaca-menu-elpa-<NAME>' name (lower-cased, non-alphanumerics → hyphens).
It accepts `(REQUEST &optional ITEM)' and fulfils the `index' / `update'
protocol used by `elpaca-menu-functions' and `:inherit'. SOURCE-FN builds
 the cells (default `elpaca-menu-elpa--source')."
  (let ((sym (or sym (intern (concat "elpaca-menu-elpa-" (elpaca-menu-elpa--name name)))))
        (source-fn (or source-fn 'elpaca-menu-elpa--source))
        (doc (format "Fulfill menu REQUEST for the %S archive." name)))
    (eval `(defun ,sym (request &optional item)
             ,doc
             (elpaca-menu-elpa--run ,name ,url request ,source-fn item)))
    sym))

;;;###autoload
(defun elpaca-menu-elpa-gnu (request &optional item)
  "Fulfill GNU ELPA menu `index' or `update' REQUEST for ITEM."
  (elpaca-menu-elpa--run "gnu" "https://elpa.gnu.org/packages/" request
                         'elpaca-menu-elpa--source item))

;;;###autoload
(defun elpaca-menu-elpa-nongnu (request &optional item)
  "Fulfill NonGNU ELPA menu `index' or `update' REQUEST for ITEM."
  (elpaca-menu-elpa--run "nongnu" "https://elpa.nongnu.org/nongnu/" request
                         'elpaca-menu-elpa--source item))

;;;###autoload
(defun elpaca-menu-melpa (request &optional item)
  "Fulfill MELPA menu `index' or `update' REQUEST for ITEM."
  (elpaca-menu-elpa--run "melpa" "https://melpa.org/packages/" request
                         'elpaca-menu-melpa--source item))

;; Back-compat aliases for the previous function names.
(defalias 'elpaca-menu-gnu-elpa    #'elpaca-menu-elpa-gnu)
(defalias 'elpaca-menu-nongnu-elpa #'elpaca-menu-elpa-nongnu)

(provide 'elpaca-menu-elpa)
;;; elpaca-menu-elpa.el ends here
