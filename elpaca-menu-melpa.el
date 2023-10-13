;;; elpaca-menu-melpa.el --- Elpaca MELPA menu support -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Nicholas Vollmer

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

;;  MELPA support for Elpaca.

;;; Code:
(require 'cl-lib)
(require 'elpaca)
(require 'url)

(defvar elpaca-menu-melpa-repo-url  "https://github.com/melpa/melpa.git" "URL for MELPA repo.")
(defvar elpaca-menu-melpa-metadata-url "https://melpa.org/archive.json" "URL for ELPA metadata.")
(defvar elpaca-menu-melpa-name "MELPA" "Display name of the ELPA.")
(defvar elpaca-menu-melpa-branch-name "master" "Name of MELPA repo branch to check out.")
(defvar elpaca-menu-melpa-index-cache
  (elpaca--read-file (expand-file-name "melpa.eld" elpaca-cache-directory))
  "MELPA recipe cache.")
(defvar elpaca-menu-melpa-metadata-formatter nil
  "Function which, given metadta, returns alist of form: ((PKG-SYMBOL . data) ...).")
(defvar url-http-end-of-headers)

(defun elpaca-menu-melpa--metadata ()
  "Return an alist of MELPA package metadata."
  (with-current-buffer (url-retrieve-synchronously elpaca-menu-melpa-metadata-url 'silent)
    (let ((s (decode-coding-region url-http-end-of-headers (point-max) 'utf-8 t)))
      (if (fboundp #'json-parse-string)
          (json-parse-string s :object-type 'alist)
        (require 'json)
        (let ((json-object-type 'alist))
          (json-read-from-string s))))))

(defun elpaca-menu-melpa--clone (path)
  "Clone MELPA recipes repo to PATH."
  (let ((default-directory path))
    (make-directory path t)
    (make-directory (expand-file-name ".git/info/" path) t)
    (message "Downloading %s recipes..." elpaca-menu-melpa-name)
    (let* ((process-environment ;; ignore user git config see: https://github.com/progfolio/elpaca/issues/129
            (append '("GIT_CONFIG_SYSTEM=/dev/null" "GIT_CONFIG_GLOBAL=/dev/null")
                    process-environment))
           (branch elpaca-menu-melpa-branch-name)
           (processes
            (list
             (elpaca-process-call "git" "init")
             (elpaca-process-call "git" "config" "core.sparseCheckout" "true")
             (with-temp-buffer
               (insert "recipes")
               (append-to-file (point-min) (point-max)
                               (expand-file-name ".git/info/sparse-checkout" path)))
             (elpaca-process-call "git" "remote" "add" "origin" elpaca-menu-melpa-repo-url)
             (elpaca-process-call "git" "pull" "--depth=1" "origin" branch)
             (elpaca-process-call "git" "checkout" branch)
             (elpaca-process-call "git" "branch" "--set-upstream-to" (format "origin/%s" branch) branch)))
           (err (car (cl-remove-if #'zerop (delq nil processes) :key #'car))))
      (when err (error "Unable to clone %s: %S" elpaca-menu-melpa-name err))
      (message "Downloading %s recipes...100%%" elpaca-menu-melpa-name))))

(defun elpaca-menu-melpa--update ()
  "Update recipes in MELPA menu."
  (message "Downloading %s..." elpaca-menu-melpa-name)
  (elpaca-with-process-call ("git" "pull")
    (message (if success "Downloading %s...100%%" "Unable to pull %s recipes") elpaca-menu-melpa-name)))

(defun elpaca-menu-melpa--convert (file metadata)
  "Return menu item candidate for FILE's MELPA recipe and METADATA."
  (with-temp-buffer
    (insert-file-contents file)
    (condition-case-unless-debug _
        (when-let ((recipe (read (buffer-string)))
                   (package (pop recipe))
                   ((member (plist-get recipe :fetcher)
                            '(git github gitlab sourcehut codeberg))))
          (setq recipe
                (append (list :package (symbol-name package)) recipe))
          (unless (plist-member recipe :files)
            (setq recipe (plist-put recipe :files elpaca-default-files-directive)))
          (let ((candidate (list :source elpaca-menu-melpa-name :recipe recipe)))
            (when-let ((data (alist-get package metadata)))
              (setq candidate
                    (append candidate
                            (list :description (alist-get 'desc data)
                                  :date
                                  (ignore-errors
                                    (when-let ((s (number-to-string
                                                   (aref (alist-get 'ver data) 0))))
                                      (date-to-time
                                       (string-join (list
                                                     (substring s 0 4)
                                                     (substring s 4 6)
                                                     (substring s 6))
                                                    "-"))))
                                  :url (alist-get 'url (alist-get 'props data))))))
            (cons (intern-soft (file-name-nondirectory file)) candidate)))
      ((error) (message "elpaca-menu-melpa could not process %S" file) nil))))

(defun elpaca-menu-melpa--index ()
  "Return candidate list of available MELPA recipes."
  (or elpaca-menu-melpa-index-cache
      (prog1 (setq elpaca-menu-melpa-index-cache
                   (cl-loop with metadata =
                            (if elpaca-menu-melpa-metadata-formatter
                                (funcall elpaca-menu-melpa-metadata-formatter
                                         (elpaca-menu-melpa--metadata))
                              (elpaca-menu-melpa--metadata))
                            for file in (directory-files "./recipes/" 'full "\\(?:\\`[^.]\\)")
                            for candidate = (elpaca-menu-melpa--convert file metadata)
                            when candidate collect candidate))
        (elpaca--write-file (expand-file-name (format "%s.eld" (downcase elpaca-menu-melpa-name))
                                              elpaca-cache-directory)
          (prin1 elpaca-menu-melpa-index-cache)))))

;;;###autoload
(defun elpaca-menu-melpa (request)
  "Delegate REQUEST.
If REQUEST is `index`, return a recipe candidate alist.
If REQUEST is `update`, update the MELPA recipe cache."
  (let* ((name (file-name-sans-extension (file-name-nondirectory elpaca-menu-melpa-repo-url)))
         (repo (expand-file-name name elpaca-cache-directory))
         (default-directory repo))
    (unless (file-exists-p repo) (elpaca-menu-melpa--clone repo))
    (pcase request
      ('index  (elpaca-menu-melpa--index))
      ('update (setq elpaca-menu-melpa-index-cache nil)
               (elpaca-menu-melpa--update)))))

(provide 'elpaca-menu-melpa)
;;; elpaca-menu-melpa.el ends here
