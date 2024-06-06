;;; elpaca-info.el --- Display package info  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2024  Nicholas Vollmer

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
                                                  (apply #'elpaca-info args)
                                                  (goto-char p)))
                                 (list id menu t))))

(defun elpaca-info--format-recipe (recipe)
  "Return formatted RECIPE."
  (with-temp-buffer
    (delay-mode-hooks (emacs-lisp-mode) (auto-fill-mode))
    (insert recipe)
    (goto-char (point-min))
    (while (re-search-forward "\\(?: :[[:alpha:]]+\\)" nil 'noerror) (replace-match "\n\\&"))
    (flush-lines "^$" (point-min) (point-max))
    (unless (= (char-after (1+ (point-min))) ?\s)
      (goto-char (1+ (point-min)))
      (insert " "))
    (goto-char (point-min))
    (while (not (eobp))
      (unless (looking-at-p ";;")
        (let ((fill-column (if (looking-at-p ".*:files") 40 fill-column)))
          (fill-region (line-beginning-position) (line-end-position))))
      (forward-line))
    (font-lock-ensure)
    (let ((inhibit-message t)) (indent-region (point-min) (point-max)))
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
                (alist-get (cl-loop for source in sources thereis
                                    (when-let ((member (plist-member (cdr source) prop))
                                               ((equal (cadr member) val)))
                                      (car source)))
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

(defun elpaca-info--button (id)
  "Return info button for package associated with ID."
  (elpaca-ui--buttonize (symbol-name id) (lambda (id) (elpaca-info id nil t)) id))

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
   for repo-exists = (and repo (file-exists-p repo)) for build-exists = (and builds (file-exists-p build))
   collect (concat (format
                    (concat "%-" longest "s")
                    (concat repos (propertize (file-relative-name repo elpaca-repos)
                                              'face (if repo-exists '(:weight bold) 'error))))
                   (if (and repo-exists build-exists) " → " " ! ")
                   builds (propertize (file-relative-name build elpaca-builds-directory)
                                      'face (if build-exists '(:weight bold) 'error)))))

(declare-function elpaca--flattened-menus "elpaca")
(defun elpaca--info (id &optional menu)
  "Return info string for item with ID in MENU."
  (cl-loop
   with menus = (elpaca-info--menus id)
   with menu = (or menu (caar menus))
   with item = (or (cdr (alist-get menu menus)) (user-error "No info for %s" id))
   with on-disk-p = (elpaca--on-disk-p id)
   with e = (elpaca-get id)
   with i =  "\n  "
   with sections =
   (list
    (list nil (propertize (symbol-name id) 'face 'elpaca-info-package))
    (list nil (concat (and (cdr menus) (format  " [%s]" (string-join (elpaca-info--source-buttons menus) "|"))) "\n"))
    (list nil (concat (plist-get item :description) "\n"))
    (list "%s %s" "source:" (plist-get item :source))
    (when-let ((url (plist-get item :url)))
      (list "%s %s" "url:" (elpaca-ui--buttonize url #'browse-url url)))
    (unless (equal (plist-get item :source) "Init file")
      (list "%s\n%s" "menu item recipe:"
            (elpaca-info--format-recipe (format "%S" (plist-get item :recipe)))))
    (list "%s\n%s" "full recipe:" (elpaca-info--recipe item))
    (when-let ((e) (pinned (elpaca--pinned-p e)))
      (list "%s %s" "pinned:" (if-let ((pinner (car pinned))
                                       ((not (eq pinner (elpaca<-id e)))))
                                  (format "implicitly by mono-repo sibling %s"
                                          (elpaca-info--button pinner))
                                (format "%s %s" (cadr pinned) (cddr pinned)))))
    (list "%s\n%s" "dependencies:"
          (if-let ((deps (ignore-errors (elpaca--dependencies (elpaca-get id) t))))
              (cl-loop
               with max = (cl-loop for (id . _) in deps maximize (length (symbol-name id)))
               with last = (caar (last deps))
               for (id . min) in deps concat
               (format (concat "  %" (number-to-string (- max)) "s >= %s" (unless (eq id last) "\n"))
                       (if (eq id 'emacs) id (elpaca-info--button id))
                       (car min)))
            (if on-disk-p "nil" (if (memq item (cl-set-difference elpaca-ignored-dependencies '(emacs elpaca)))
                                    "built-in" "?"))))
    (list "%s %s" "dependents:"
          (if-let ((ds (remq 'emacs (elpaca--dependents id 'noerror))))
              (concat i (mapconcat #'elpaca-info--button (cl-sort ds #'string<) i))
            (if on-disk-p "nil" "?")))
    (when-let ((version (if-let ((e)
                                 (default-directory (elpaca<-repo-dir e))
                                 (v (ignore-errors (or (elpaca--declared-version e) (elpaca-latest-tag e)))))
                            (concat (string-trim v) " "
                                    (ignore-errors
                                      (string-trim (elpaca-process-output "git" "rev-parse" "--short" "HEAD"))))
                          (when-let ((builtin (alist-get id package--builtin-versions)))
                            (concat (mapconcat #'number-to-string builtin ".") " (builtin)")))))
      (list "%s %s" "installed version:" version))
    (when-let ((e) (statuses (elpaca<-statuses e))) (list "%s\n  %S" "statuses:" statuses))
    (when-let ((e) (files (ignore-errors (elpaca--files e))))
      (list "%s\n  %s" "files:" (string-join (elpaca-info--files files) i)))
    (when-let ((e) (log (elpaca<-log e)))
      (list "%s\n%s" "log:"
            (cl-loop for (_ time info _) in (reverse log) concat
                     (format "  %s %s\n" (propertize (format "[%s]" (format-time-string "%F %T" time))
                                                     'face 'font-lock-comment-face)
                             info)))))
   for (spec heading data) in sections concat
   (if spec (format (concat "\n" spec) (propertize heading 'face 'elpaca-info-section) data) heading)))

;;;###autoload
(defun elpaca-info (id &optional menu interactive)
  "Return package info for ID from MENU.
If INTERACTIVE is non-nil, display info in a dedicated buffer."
  (interactive (if-let ((elpaca-overriding-prompt "Package info: ")
                        (items (append (elpaca--custom-candidates t) (elpaca--flattened-menus)))
                        (item (elpaca-menu-item t items)))
                   (list (car item) (elpaca-item-menu item) t)))
  (let ((info (elpaca--info id menu)))
    (if (not interactive)
        (substring-no-properties info)
      (let* ((e (elpaca-get id)))
        (with-current-buffer (get-buffer-create "*elpaca-info*")
          (unless (derived-mode-p 'elpaca-info-mode) (elpaca-info-mode))
          (with-silent-modifications
            (erase-buffer)
            (when e (setq default-directory (elpaca<-repo-dir e)))
            (insert info)
            (goto-char (point-min))
            (pop-to-buffer (current-buffer))))))))

(provide 'elpaca-info)
;;; elpaca-info.el ends here
