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
(define-derived-mode elpaca-info-mode special-mode "elpaca-info-edit"
  "Major mode for editing speedo attempts.

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
   for cell in files collect
   (concat (format (concat "%-" longest "s")
                   (concat repos (propertize (file-relative-name (car cell) elpaca-repos)
                                             'face '(:weight bold))))
           " → "
           builds (propertize (file-relative-name (cdr cell) elpaca-builds-directory)
                              'face '(:weight bold)))))

(defun elpaca-info--section (spec heading data)
  "Return section for HEADING with DATA formatted according to SPEC."
  (format spec (propertize heading 'face 'elpaca-finished) data))

(defun elpaca-info--print (item)
  "Print info for ITEM."
  (read-only-mode -1)
  (erase-buffer)
  (setq-local elpaca--info
              (mapcar #'cdr (cl-remove-if-not (lambda (it) (eq it item))
                                              (append (elpaca--custom-candidates)
                                                      (elpaca--menu-items))
                                              :key #'car))
              elpaca-info--item item)
  (when (> elpaca-info--source-index (1- (length elpaca--info)))
    (setq elpaca-info--source-index 0))
  (let* ((info (nth elpaca-info--source-index elpaca--info))
         (recipe (plist-get info :recipe))
         (on-disk-p (elpaca--on-disk-p item))
         (e (elpaca-get item))
         (sections
          (list
           (elpaca-info--section "%s %s" "source:" (plist-get info :source))
           (when-let ((url (plist-get info :url)))
             (elpaca-info--section "%s %s" "url:" (elpaca-ui--buttonize url #'browse-url url)))
           (unless (equal (plist-get info :source) "Init file")
             (elpaca-info--section "%s\n%s" "menu item recipe:"
                                   (elpaca-info--format-recipe (plist-get info :recipe))))
           (when e (elpaca-info--section "%s\n%s" "computed recipe:"
                                         (elpaca-info--format-recipe (elpaca<-recipe e))))
           (elpaca-info--section
            "%s %s" "dependencies:"
            (if-let ((ds (remq 'emacs (elpaca-dependencies item))))
                (concat "\n  " (string-join (elpaca-info--buttons (cl-sort ds #'string<)) "\n  "))
              (if on-disk-p "n/a" "?")))
           (elpaca-info--section
            "%s %s" "dependents:"
            (if-let ((ds (remq 'emacs (elpaca-dependents item))))
                (concat "\n  " (string-join (elpaca-info--buttons (cl-sort ds #'string<)) "\n  "))
              (if on-disk-p "n/a" "?")))
           (when-let ((e) (files (elpaca--files e)))
             (elpaca-info--section "%s\n  %s" "files:" (string-join (elpaca-info--files files)
                                                                    "\n  "))))))
    (insert (propertize (plist-get recipe :package) 'face '(:height 2.0))
            " [" (string-join (elpaca-info--source-buttons elpaca--info) "|") "]\n\n"
            (plist-get info :description) "\n\n"
            (string-join (delq nil sections) "\n")))
  (goto-char (point-min)))

;;;###autoload
(defun elpaca-info (item)
  "Display package info for ITEM in a dedicated buffer."
  (interactive (list (let ((elpaca-overriding-prompt "Package info: ")
                           (menus (cons #'elpaca--custom-candidates elpaca-menu-functions)))
                       (intern (plist-get (elpaca-menu-item t nil menus) :package)))))
  (with-current-buffer (get-buffer-create "*elpaca-info*")
    (unless (derived-mode-p 'elpaca-info-mode)
      (elpaca-info-mode)
      (setq-local elpaca-info--source-index 0))
    (elpaca-info--print item)
    (pop-to-buffer (current-buffer))))

(provide 'elpaca-info)
;;; elpaca-info.el ends here
