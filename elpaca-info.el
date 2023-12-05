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

(defun elpaca-info--menus (id)
  "Return alist of menus filtered for item matching ID."
  (cl-loop with cache = (copy-tree elpaca--menu-cache)
           for (fn . items) in (push (cons 'init-file (elpaca--custom-candidates t)) cache)
           for found  = (cl-find id items :key #'car)
           when found collect (cons fn found)))

(defun elpaca-info--source-buttons (menus)
  "Return list of package source buttons from MENUS."
  (cl-loop for (menu . (id . props)) in menus collect
           (elpaca-ui--buttonize (plist-get props :source)
                                 (lambda (args) (let ((p (point)))
                                                  (apply #'elpaca-info--print args)
                                                  (goto-char p)))
                                 (list id menu))))

(defun elpaca-info--format-recipe (recipe)
  "Return formatted RECIPE."
  (with-temp-buffer
    (delay-mode-hooks (emacs-lisp-mode) (auto-fill-mode))
    (insert recipe)
    (goto-char (point-min))
    (while (re-search-forward "\\(?: :[[:alpha:]]+\\)" nil 'noerror) (replace-match "\n\\&"))
    (unless (= (char-after (1+ (point-min))) ?\s)
      (goto-char (1+ (point-min)))
      (insert " "))
    (flush-lines "^$")
    (goto-char (point-min))
    (while (not (eobp)) (let ((eol (line-end-position)))
                          (unless (looking-at ";;" eol)
                            (let ((fill-column (if (looking-at ".*:files" eol) 40 fill-column)))
                              (fill-region (line-beginning-position) (line-end-position))))
                          (forward-line)))
    (font-lock-ensure)
    (indent-region (point-min) (point-max))
    (string-trim (buffer-string))))

(defun elpaca-info--recipe (item)
  "Print annotated recipe, considering menu ITEM."
  (let* ((item-recipe (plist-get item :recipe))
         (id (intern (plist-get item-recipe :package)))
         (e (elpaca-get id))
         (order (if e (elpaca<-order e) id))
         (declared (cdr-safe order)))
    (with-temp-buffer
      (delay-mode-hooks (emacs-lisp-mode) (auto-fill-mode))
      (cl-loop
       with ordered = '( nil elpaca-recipe-functions declaration
                         elpaca-order-functions elpaca-menu-item)
       with order-mods = (run-hook-with-args-until-success 'elpaca-order-functions order)
       with recipe-mods =
       (run-hook-with-args-until-success
        'elpaca-recipe-functions (elpaca-merge-plists item-recipe order-mods declared))
       with recipe = (elpaca-merge-plists item-recipe order-mods declared recipe-mods
                                          `(:package ,(plist-get item-recipe :package)))
       with lookup = `((nil ,(format "(:package %S " (plist-get recipe :package))))
       with sources = `((elpaca-recipe-functions . ,recipe-mods) (declaration . ,declared)
                        (elpaca-order-functions .  ,order-mods)
                        (elpaca-menu-item . ,item-recipe))
       for (prop val) on recipe by #'cddr
       unless (or (eq prop :package) (and (eq prop :source) (null val)))
       do (push (format " %s %S" prop
                        (if (equal val elpaca-default-files-directive) '(:defaults) val))
                (alist-get (cl-some (lambda (source)
                                      (when-let ((member (plist-member (cdr source) prop))
                                                 ((equal (cadr member) val)))
                                        (car source)))
                                    sources)
                           lookup))
       finally return
       (elpaca-info--format-recipe
        (concat (cl-loop for source in ordered for vals = (alist-get source lookup)
                         when (and source vals
                                   (not (and (eq source 'elpaca-menu-item)
                                             (equal (plist-get item :source) "Init file"))))
                         concat (format "\n;; Inherited from %s.\n" source)
                         when vals concat (string-join vals "\n"))
                ")"))))))

(defun elpaca-info--buttons (ids)
  "Return list of buttons from IDS."
  (mapcar (lambda (id) (elpaca-ui--buttonize (symbol-name id)
                                             (lambda (id) (elpaca-info--print id))
                                             id))
          ids))

(defun elpaca-info--files (files)
  "Return list of formatted FILES strings."
  (cl-loop
   with elpaca-repos = (expand-file-name "./repos/" elpaca-directory)
   with repos =  (propertize "$REPOS/" 'help-echo elpaca-repos)
   with longest = (number-to-string
                   (- (cl-loop for (source . _) in files maximize (length source))
                      (- (length elpaca-repos) (length repos))))
   with builds = (propertize "$BUILDS/" 'help-echo elpaca-builds-directory)
   for (repo . build) in files
   for repo-exists = (file-exists-p repo) for build-exists = (file-exists-p build)
   collect (concat (format
                    (concat "%-" longest "s")
                    (concat repos (propertize (file-relative-name repo elpaca-repos)
                                              'face (if repo-exists '(:weight bold) 'error))))
                   (if (and repo-exists build-exists) " → " " ! ")
                   builds (propertize (file-relative-name build elpaca-builds-directory)
                                      'face (if build-exists '(:weight bold) 'error)))))

(defun elpaca-info--section (spec heading data)
  "Return section for HEADING with DATA formatted according to SPEC."
  (format spec (propertize heading 'face 'elpaca-info-section) data))

(declare-function elpaca--flattened-menus "elpaca")
(defun elpaca-info--print (id &optional menu)
  "Print info for item with ID in MENU."
  (with-silent-modifications
    (erase-buffer)
    (let* ((menus (elpaca-info--menus id))
           (menu (or menu (caar menus)))
           (item (cdr (alist-get menu menus)))
           (recipe (plist-get item :recipe))
           (on-disk-p (elpaca--on-disk-p id))
           (e (elpaca-get id))
           (i  "\n  ")
           (sections
            (list
             (elpaca-info--section "%-7s %s" "source:" (plist-get item :source))
             (when-let ((url (plist-get item :url)))
               (elpaca-info--section "%-7s %s" "url:" (elpaca-ui--buttonize url #'browse-url url)))
             (unless (equal (plist-get item :source) "Init file")
               (elpaca-info--section "%s\n%s" "menu item recipe:"
                                     (elpaca-info--format-recipe
                                      (format "%S" (plist-get item :recipe)))))
             (elpaca-info--section "%s\n%s" "full recipe:" (elpaca-info--recipe item))
             (elpaca-info--section
              "%s %s" "dependencies:"
              (if-let ((ds (remq 'emacs (ignore-errors (elpaca-dependencies id)))))
                  (concat i (string-join (elpaca-info--buttons (cl-sort ds #'string<)) i))
                (if on-disk-p "nil"
                  (if (memq item (cl-set-difference elpaca-ignored-dependencies '(emacs elpaca)))
                      "built-in" "?"))))
             (elpaca-info--section
              "%s %s" "dependents:"
              (if-let ((ds (remq 'emacs (elpaca--dependents id 'noerror))))
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
      (when (> (length menus) 1)
        (insert " [" (string-join (elpaca-info--source-buttons menus) "|") "]"))
      (insert "\n" (or (plist-get item :description) "n/a") "\n\n"
              (string-join (delq nil sections) "\n")))
    (goto-char (point-min))))

;;;###autoload
(defun elpaca-info (id &optional menu)
  "Display package info for ID from MENU in a dedicated buffer."
  (interactive (if-let ((elpaca-overriding-prompt "Package info: ")
                        (items (append (elpaca--custom-candidates t) (elpaca--flattened-menus)))
                        (item (elpaca-menu-item t items)))
                   (list (car item) (elpaca-item-menu item))))
  (with-current-buffer (get-buffer-create "*elpaca-info*")
    (unless (derived-mode-p 'elpaca-info-mode) (elpaca-info-mode))
    (elpaca-info--print id menu)
    (pop-to-buffer (current-buffer))))

(provide 'elpaca-info)
;;; elpaca-info.el ends here
