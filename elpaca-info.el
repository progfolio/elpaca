;;; elpaca-info.el --- Display package info  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2025  Nicholas Vollmer

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

\\{elpaca-info-mode-map}"
  :interactive nil)

(defun elpaca-info--menus (id)
  "Return alist of menus filtered for item matching ID."
  (cl-loop for menu in elpaca-menu-functions
           for found = (funcall menu 'index id)
           when found collect (append (list menu id) found)))

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
                                    (when-let* ((member (plist-member (cdr source) prop))
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

(defvar-local elpaca-info-alist nil)
(defun elpaca--info (id &optional menu)
  "Return info string for item with ID in MENU."
  (let* ((menus (elpaca-info--menus id))
         (menu (or menu (caar menus)))
         (item (or (cdr (alist-get menu menus)) (user-error "No info for %s" id))))
    (with-temp-buffer
      (setf elpaca-info-alist
            `((id . ,id) (menus . ,menus) (menu .  ,menu) (item . ,item)
              (on-disk-p ,(elpaca--on-disk-p id)) (e . , (elpaca-get id)) (indentation . "\n  ")))
      (run-hooks 'elpaca-info-sections-hook)
      (buffer-string))))

(defmacro elpaca-info-defsection (name &rest body)
  "Define section function with NAME which evals BODY to return section string."
  (declare (indent 1))
  `(defun ,(intern (format "elpaca-info-section--%s" name)) ()
     ,(format "Insert package's %s." name)
     (let-alist elpaca-info-alist
       (let ((s ,(macroexp-progn body))) (when (stringp s) (insert s))))))

(defun elpaca-info--header (string)
  "Return propertized, formatted section header STRING."
  (propertize (concat string ":") 'face 'elpaca-info-section))

(elpaca-info-defsection title
  (propertize (symbol-name .id) 'face `(,@(when-let* ((.e) (status (elpaca--status .e)))
                                            (list (alist-get status elpaca-status-faces)))
                                        elpaca-info-package)))
(elpaca-info-defsection menu-buttons
  (concat (and (cdr .menus) (format  " [%s]" (string-join (elpaca-info--source-buttons .menus) "|"))) "\n"))
(elpaca-info-defsection description (concat (plist-get .item :description) "\n\n"))
(elpaca-info-defsection source (format "%s %s" (elpaca-info--header "source") (plist-get .item :source)))
(elpaca-info-defsection url
  (when-let* ((url (plist-get .item :url)))
    (insert (format "\n%s %s" (elpaca-info--header "url") (elpaca-ui--buttonize url #'browse-url url)))))
(elpaca-info-defsection menu-item
  (unless (equal (plist-get .item :source) "Init file")
    (format "\n%s\n%s" (elpaca-info--header "menu item recipe")
            (elpaca-info--format-recipe (format "%S" (plist-get .item :recipe))))))
(elpaca-info-defsection recipe
  (format "\n%s\n%s" (elpaca-info--header "full recipe") (elpaca-info--recipe .item)))
(elpaca-info-defsection pin-status
  (when-let* ((.e) (pinned (elpaca-pinned-p .e)))
    (format "\n%s %s" (elpaca-info--header "pinned") (if-let* ((pinner (car pinned))
                                                               ((not (eq pinner (elpaca<-id .e)))))
                                                         (format "implicitly by mono-repo sibling %s"
                                                                 (elpaca-info--button pinner))
                                                       (format "%s %s" (cadr pinned) (cddr pinned))))))
(elpaca-info-defsection dependencies
  (format "\n%s\n%s" (elpaca-info--header "dependencies")
          (if-let* ((deps (ignore-errors (elpaca--dependencies (elpaca-get .id) t))))
              (cl-loop
               with max = (cl-loop for (id . _) in deps maximize (length (symbol-name id)))
               with last = (caar (last deps))
               for (id . min) in deps concat
               (format (concat "  %" (number-to-string (- max)) "s >= %s" (unless (eq id last) "\n"))
                       (if (eq id 'emacs) id (elpaca-info--button id))
                       (car min)))
            (if .on-disk-p "nil" (if (memq .item (cl-set-difference elpaca-ignored-dependencies '(emacs elpaca)))
                                     "built-in" "?")))))
(elpaca-info-defsection dependents
  (format "\n%s %s" (elpaca-info--header "dependents")
          (if-let* ((ds (remq 'emacs (elpaca--dependents .id 'noerror))))
              (concat .indentation (mapconcat #'elpaca-info--button (cl-sort ds #'string<) .indentation))
            (if .on-disk-p "nil" "?"))))
(elpaca-info-defsection version
  (when-let* ((version (if-let* ((.e)
                                 (default-directory (elpaca<-repo-dir .e))
                                 (v (ignore-errors (or (elpaca--declared-version .e) (elpaca-latest-tag .e)))))
                           (concat (string-trim v) " "
                                   (ignore-errors
                                     (string-trim (elpaca-process-output "git" "rev-parse" "--short" "HEAD"))))
                         (when-let* ((builtin (alist-get .id package--builtin-versions)))
                           (concat (mapconcat #'number-to-string builtin ".") " (builtin)")))))
    (format "\n%s %s" (elpaca-info--header "installed version") version)))
(elpaca-info-defsection statuses
  (when-let* ((.e) (statuses (elpaca<-statuses .e)))
    (format "\n%s\n  %S" (elpaca-info--header "statuses") statuses)))
(elpaca-info-defsection files
  (when-let* ((.e) (files (ignore-errors (elpaca--files .e))))
    (format "\n%s\n  %s" (elpaca-info--header "files") (string-join (elpaca-info--files files) .indentation))))
(elpaca-info-defsection log
  (when-let* ((.e) (log (elpaca<-log .e)))
    (format "\n%s\n%s" (elpaca-info--header "log")
            (cl-loop for (status time info _) in (reverse log) concat
                     (format "  %s %s\n" (propertize (format "[%s]" (format-time-string "%F %T" time))
                                                     'face (alist-get status elpaca-status-faces 'font-lock-comment-face))
                             info)))))
(defcustom elpaca-info-sections-hook
  '( elpaca-info-section--title elpaca-info-section--menu-buttons elpaca-info-section--description
     elpaca-info-section--source elpaca-info-section--url elpaca-info-section--menu-item
     elpaca-info-section--recipe elpaca-info-section--pin-status elpaca-info-section--dependencies
     elpaca-info-section--dependents elpaca-info-section--version elpaca-info-section--statuses
     elpaca-info-section--files elpaca-info-section--log)
  "Hook run to layout info buffer." :type 'hook :group 'elpaca)

;;;###autoload
(defun elpaca-info (id &optional menu interactive)
  "Return package info for ID from MENU.
If INTERACTIVE is non-nil, display info in a dedicated buffer."
  (interactive (if-let* ((elpaca-overriding-prompt "Package info: ")
                         (item (elpaca-menu-item)))
                   (list (car item)
                         (car (cl-find (cdr item)
                                       (cl-loop for menu in elpaca-menu-functions collect (cons menu (funcall menu 'index)))
                                       :key #'cdr :test #'rassoc))
                         t)))
  (let ((info (elpaca--info id menu)))
    (if (not interactive)
        (substring-no-properties info)
      (let* ((e (elpaca-get id)))
        (with-current-buffer (get-buffer-create "*elpaca-info*")
          (setq-local revert-buffer-function (lambda (&rest _) (elpaca-info id menu t)))
          (unless (derived-mode-p 'elpaca-info-mode) (elpaca-info-mode))
          (with-silent-modifications
            (erase-buffer)
            (when e (setq default-directory (elpaca<-repo-dir e)))
            (insert info)
            (goto-char (point-min))
            (pop-to-buffer (current-buffer))))))))

(provide 'elpaca-info)
;;; elpaca-info.el ends here
