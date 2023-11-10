;;; elpaca-info.el --- Display package info  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Nicholas Vollmer

;; Author:  Nicholas Vollmer
;; Keywords: convenience

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

;;; Code:
(require 'elpaca)
(require 'elpaca-ui)

(defface elpaca-info-section '((t (:weight bold)))
  "Marks a section of the elpaca-info-buffer." :group 'elpaca-ui)
(defface elpaca-info-package '((t (:height 2.0)))
  "The name of the package in `elpaca-info-mode'." :group 'elpaca-ui)

(defvar-local elpaca--info nil)
(defvar-local elpaca-info--item nil)
(defvar-local elpaca-info--source-index nil)
(defvar elpaca-info-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "TAB")   'forward-button)
    (define-key m (kbd "<tab>")   'forward-button)
    (define-key m (kbd "S-TAB") 'backward-button)
    (define-key m (kbd "<backtab>") 'backward-button)
    (define-key m (kbd "i") 'elpaca-info)
    m))
(define-derived-mode elpaca-info-mode special-mode "elpaca-info-mode"
  "Major mode for viewing Elpaca package info.

\\{elpaca-info-mode-map}")

(defun elpaca-info--format-recipe (recipe)
  "Return formatted RECIPE string."
  (with-temp-buffer
    (insert "( "
            (string-join
             (cl-loop for (key val) on recipe by #'cddr
                      when (eq key :files) do
                      (let ((files (cl-set-difference val elpaca-default-files-directive
                                                      :test #'equal)))
                        (unless (memq :defaults files) (setq val (push :defaults files))))
                      collect (format (concat "%-10S" (if (eq key :files) "\n" " ") "%S\n")
                                      key val)
                      into pairs
                      finally (setf (car (last pairs)) (string-trim (car (last pairs))))
                      finally return pairs))
            ")")
    (delay-mode-hooks (emacs-lisp-mode))
    (let ((inhibit-message t)) (indent-region (point-min) (point-max)))
    (font-lock-ensure)
    (string-trim (buffer-string))))

(defun elpaca-info--source-buttons (info)
  "Return list of package source buttons from INFO."
  (cl-loop for i below (length info)
           for spec = (nth i info)
           for item = (intern (plist-get (plist-get spec :recipe) :package))
           collect (elpaca-ui--buttonize (plist-get spec :source)
                                         (lambda (args)
                                           (setq elpaca-info--source-index (car args))
                                           (let ((p (point)))
                                             (elpaca-info--print (cdr args))
                                             (goto-char p)))
                                         (cons i item))))

(defun elpaca-info--buttons (items)
  "Return list of buttons from ITEMS."
  (mapcar (lambda (i) (elpaca-ui--buttonize (symbol-name i)
                                            (lambda (i)
                                              (setq elpaca-info--source-index 0)
                                              (elpaca-info--print i))
                                            i))
          items))

(defun elpaca-info--files (files)
  "Return list of formatted FILES strings."
  (cl-loop
   with elpaca-repos = (expand-file-name "./repos/" elpaca-directory)
   with repos =  (propertize "$REPOS/" 'help-echo elpaca-repos)
   with longest = (number-to-string
                   (- (cl-loop for (source . _) in files maximize (length source))
                      (- (length elpaca-repos) (length repos))))
   with builds = (propertize "$BUILDS/" 'help-echo elpaca-builds-directory)
   for (repo . build) in files collect
   (concat
    (format
     (concat "%-" longest "s")
     (concat repos (propertize (file-relative-name repo elpaca-repos)
                               'face (if (file-exists-p repo) '(:weight bold) 'error))))
    " → " builds (propertize (file-relative-name build elpaca-builds-directory)
                             'face (if (file-exists-p build) '(:weight bold) 'error)))))

(defun elpaca-info--section (spec heading data)
  "Return section for HEADING with DATA formatted according to SPEC."
  (format spec (propertize heading 'face 'elpaca-info-section) data))

(defun elpaca-info--print (item)
  "Print info for ITEM."
  (with-silent-modifications
    (erase-buffer)
    (setq-local elpaca--info
                (mapcar #'cdr (cl-remove-if-not (lambda (it) (eq it item))
                                                (append (elpaca--custom-candidates t)
                                                        (elpaca--flattened-menus))
                                                :key #'car))
                elpaca-info--item item)
    (when (> elpaca-info--source-index (1- (length elpaca--info)))
      (setq elpaca-info--source-index 0))
    (let* ((info (nth elpaca-info--source-index elpaca--info))
           (recipe (plist-get info :recipe))
           (on-disk-p (elpaca--on-disk-p item))
           (e (elpaca-get item))
           (i  "\n  ")
           (sections
            (list
             (elpaca-info--section "%-7s %s" "source:" (plist-get info :source))
             (when-let ((url (plist-get info :url)))
               (elpaca-info--section "%-7s %s" "url:" (elpaca-ui--buttonize url #'browse-url url)))
             (unless (equal (plist-get info :source) "Init file")
               (elpaca-info--section "%s\n%s" "menu item:"
                                     (elpaca-info--format-recipe (plist-get info :recipe))))
             (when-let ((i (and e (elpaca<-order e))))
               (elpaca-info--section
                "%s\n%s" "recipe:"
                (elpaca-info--format-recipe
                 (elpaca-merge-plists (append `(:package ,(symbol-name (car-safe i))) (cdr-safe i))
                                      (elpaca<-recipe e)))))
             (elpaca-info--section
              "%s %s" "dependencies:"
              (if-let ((ds (remq 'emacs (ignore-errors (elpaca-dependencies item)))))
                  (concat i (string-join (elpaca-info--buttons (cl-sort ds #'string<)) i))
                (if on-disk-p "nil"
                  (if (memq item (cl-set-difference elpaca-ignored-dependencies '(emacs elpaca)))
                      "built-in" "?"))))
             (elpaca-info--section
              "%s %s" "dependents:"
              (if-let ((ds (remq 'emacs (elpaca--dependents item 'noerror))))
                  (concat i (string-join (elpaca-info--buttons (cl-sort ds #'string<)) i))
                (if on-disk-p "nil" "?")))
             (when e (elpaca-info--section
                      "%s %s" "commit: "
                      (let ((default-directory (elpaca<-repo-dir e)))
                        (string-trim (or (ignore-errors (elpaca-process-output
                                                         "git" "rev-parse"  "--short" "HEAD"))
                                         "")))))
             (when-let ((e) (statuses (elpaca<-statuses e)))
               (elpaca-info--section "%s\n  %S" "statuses:" statuses))
             (when-let ((e) (files (ignore-errors (elpaca--files e))))
               (elpaca-info--section "%s\n  %s" "files:" (string-join (elpaca-info--files files) i))))))
      (when e (setq default-directory (elpaca<-repo-dir e)))
      (insert (propertize (plist-get recipe :package) 'face 'elpaca-info-package))
      (when (> (length elpaca--info) 1)
        (insert " [" (string-join (elpaca-info--source-buttons elpaca--info) "|") "]"))
      (insert "\n" (plist-get info :description) "\n\n" (string-join (delq nil sections) "\n")))
    (goto-char (point-min))))

;;;###autoload
(defun elpaca-info (item &optional source)
  "Display package info for ITEM from SOURCE in a dedicated buffer."
  (interactive (if-let ((elpaca-overriding-prompt "Package info: ")
                        (items (append (elpaca--custom-candidates t) (elpaca--flattened-menus)))
                        (item (elpaca-menu-item t items)))
                   (list (car item) (plist-get (cdr item) :source))))
  (with-current-buffer (get-buffer-create "*elpaca-info*")
    (unless (derived-mode-p 'elpaca-info-mode) (elpaca-info-mode))
    (setq-local elpaca-info--source-index
                (or (cl-position source (cl-remove-if-not (lambda (it) (eq (car it) item))
                                                          (append (elpaca--custom-candidates t)
                                                                  (elpaca--flattened-menus)))
                                 :key (lambda (item) (plist-get (cdr item) :source))
                                 :test #'equal)
                    0))
    (elpaca-info--print item)
    (pop-to-buffer (current-buffer))))

(provide 'elpaca-info)
;;; elpaca-info.el ends here
