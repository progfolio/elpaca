;;; elpaca-ui.el --- package UI for elpaca.el  -*- lexical-binding: t; -*-

;;; Commentary:
;; Package search, maintenance UI.
(require 'elpaca)
(require 'url)
(require 'tabulated-list)

;;; Code:
;;;;Faces:
(defface elpaca-ui-package
  '((default :inherit default))
  "Default face for packages."
  :group 'elpaca-faces)

(defface elpaca-ui-marked-package
  '((default (:inherit default :weight bold :foreground "pink")))
  "Face for marked packages."
  :group 'elpaca-faces)

(defgroup elpaca-ui nil
  "Elpaca's UI options."
  :group 'elpaca-ui
  :prefix "elpaca-ui-")

;;;; Customizations:
(defcustom elpaca-ui-default-query ".*"
  "Search query used when enabling `elpaca-ui-mode'."
  :type 'string)

(defcustom elpaca-ui-actions
  '(("delete"  "💀" (:inherit default :weight bold :foreground "#FF0022")
     (lambda (i) (elpaca-delete-package 'force nil i)))
    ("install" "⚙️" (:inherit default :weight bold :foreground "#89cff0") elpaca-try-package)
    ("rebuild" "♻️️" (:inherit default :weight bold :foreground "#f28500") elpaca-rebuild-package))
  "List of actions which can be taken on packages.
Each element is of the form: (DESCRIPTION PREFIX FACE FUNCTION)."
  :type 'list)

(defcustom elpaca-ui-search-tags
  '((dirty     . (lambda (entires) (cl-remove-if-not #'elpaca-worktree-dirty-p entries :key #'car)))
    (declared  . (lambda (entries) (cl-remove-if-not #'elpaca-declared-p entries :key #'car)))
    (orphan    . (lambda (entries) (cl-remove-if-not #'elpaca-ui--orphan-p entries :key #'car)))
    (unique    . (lambda (entries) (cl-remove-duplicates entries :key #'car :from-end t)))
    (random    . (lambda (entries)
                   (if (< (length entries) 10)
                       entries
                     (let ((results nil)
                           (seen nil))
                       (while (< (length results) 10)
                         (when-let ((n (random (length entries)))
                                    ((not (memq n seen))))
                           (push (nth n entries) results)
                           (push n seen)))
                       results))))
    (installed . (lambda (entries) (cl-remove-if-not #'elpaca-installed-p entries :key #'car)))
    (marked    . (lambda (entries) (cl-loop for (item . rest) in elpaca-ui--marked-packages
                                            collect (assoc item entries)))))
  "Alist of search tags.
Each cell is of form (NAME FILTER).
If FILTER is a function it must accept a single candidate as its sole
argument and return non-nil if the package is to be kept.

Each tag can be inverted in the minibuffer by prepending an
exclamation point to it. e.g. #!installed."
  :group 'elpaca-ui
  :type 'alist)

(defcustom elpaca-ui-search-debounce-interval 0.25
  "Length of time to wait before updating the search UI.
See `run-at-time' for acceptable values."
  :group 'elpaca-ui
  :type (or 'string 'int 'float))

;;;; Variables:
(defvar-local elpaca-ui--search-timer nil "Timer to debounce search input.")
(defvar-local elpaca-ui--marked-packages nil
  "List of marked packages. Each element is a cons of (PACKAGE . ACTION).")
(defvar elpaca-ui-mode-map (let ((m (make-sparse-keymap)))
                             (define-key m (kbd ":") 'elpaca-ui-send-input)
                             (define-key m (kbd "B") 'elpaca-ui-browse-package)
                             (define-key m (kbd "I") 'elpaca-ui-search-installed)
                             (define-key m (kbd "M") 'elpaca-ui-search-marked)
                             (define-key m (kbd "O") 'elpaca-ui-search-orphans)
                             (define-key m (kbd "P") 'elpaca-ui-search-previous)
                             (define-key m (kbd "R") 'elpaca-ui-search-refresh)
                             (define-key m (kbd "U") 'elpaca-ui-search-undeclared)
                             (define-key m (kbd "b") 'elpaca-ui-visit-build)
                             (define-key m (kbd "d") 'elpaca-ui-mark-delete)
                             (define-key m (kbd "i") 'elpaca-ui-mark-install)
                             (define-key m (kbd "l") 'elpaca-log)
                             (define-key m (kbd "m") 'elpaca-manager)
                             (define-key m (kbd "r") 'elpaca-ui-mark-rebuild)
                             (define-key m (kbd "s") 'elpaca-ui-search)
                             (define-key m (kbd "t") 'elpaca-status)
                             (define-key m (kbd "u") 'elpaca-ui-unmark)
                             (define-key m (kbd "v") 'elpaca-ui-visit-repo)
                             (define-key m (kbd "x") 'elpaca-ui-execute-marks)
                             m)
  "Keymap for `elpaca-ui-mode'.")

(defvar-local elpaca-ui--previous-minibuffer-contents ""
  "Keep track of minibuffer contents changes.
Allows for less debouncing than during `post-command-hook'.")
(defvar-local elpaca-ui--want-faces t "When non-nil, faces are applied to packages.")
(defvar-local elpaca-ui-search-filter nil "Filter for package searches.")
(defvar-local elpaca-ui-search-history nil "List of previous search queries.")
(defvar-local elpaca-ui-header-line-prefix nil "Header line prefix.")
(defvar-local elpaca-ui-header-line-function #'elpaca-ui--header-line
  "Function responsible for setting the UI buffer's `header-line-format'.
It recieves one argument, the parsed search query list.")
(defvar-local elpaca-ui-entries-function nil
  "Function responsible for returning the UI buffer's `tabulated-list-entries'.")
(defvar url-http-end-of-headers)

;;;; Functions:
(defun elpaca-ui--header-line (parsed &optional prefix)
  "Set `header-line-format' to reflect PARSED query.
If PREFIX is non-nil it is displayed before the rest of the header-line."
  (let* ((tags (car parsed))
         (cols (cadr parsed))
         (full-match-p (= (length cols) 1))
         (queries (and full-match-p (car cols))))
    (setq header-line-format
          (concat
           (or prefix "")
           (propertize (format " (%d matches) " (length tabulated-list-entries))
                       'face '(:weight bold))
           " "
           (concat
            (propertize
             (if full-match-p "Matching:" "Columns Matching:")
             'face 'elpaca-failed)
            " "
            (if full-match-p
                (mapconcat (lambda (cell) (concat (when (cdr cell) "!") (car cell)))
                           queries ", ")
              (mapconcat
               (lambda (col)
                 (format "|%s"
                         (if col
                             (mapconcat
                              (lambda (cell)
                                (concat (when (cdr cell) "!") (car cell)))
                              col ", ")
                           "*")))
               cols)))
           (when tags
             (concat " " (propertize "Tags:" 'face 'elpaca-failed) " "
                     (mapconcat (lambda (tag)
                                  (let ((name (car tag)))
                                    (concat (when (cdr tag) "!") "#" name)))
                                tags ", ")))))))

(defun elpaca-ui--orphan-p (item)
  "Return non-nil if ITEM's repo or build are on disk without having been queued."
  (let ((queued (elpaca--queued)))
    (unless (alist-get item queued)
      (let* ((recipe (elpaca-recipe item))
             (repo   (elpaca-repo-dir recipe)))
        (unless (cl-some (lambda (cell)
                           (when-let ((e (cdr cell))
                                      ((equal repo (elpaca<-repo-dir e))))
                             e))
                         queued)
          (or (file-exists-p (elpaca-build-dir recipe))
              (file-exists-p (elpaca-repo-dir  recipe))))))))

(defun elpaca-ui-tag-orphan (candidate)
  "Return non-nil if CANDIDATE is an oprhaned package."
  (elpaca-ui--orphan-p (car candidate)))

(defun elpaca-ui--fallback-date (e)
  "Return time of last modification for E's built elisp, otherwise nil."
  (file-attribute-modification-time
   (file-attributes (expand-file-name (concat (elpaca<-package e) ".el")
                                      (elpaca<-build-dir e)))))

(defun elpaca-ui--custom-candidates ()
  "Return declared candidate list with no recipe in `elpaca-menu-functions'."
  (cl-loop for (item . p) in (elpaca--queued)
           unless (elpaca-menu-item nil item nil nil 'no-descriptions)
           collect (list item :source "Init file"
                         :date (ignore-errors (elpaca-ui--fallback-date p))
                         :description "Not available in menu functions")))

(define-derived-mode elpaca-ui-mode tabulated-list-mode "elpaca-ui"
  "Major mode to manage packages."
  (add-hook 'minibuffer-setup-hook 'elpaca-ui--minibuffer-setup)
  (hl-line-mode))

(defun elpaca-ui--minibuffer-setup ()
  "Set up the minibuffer for live filtering."
  (let ((continue nil)
        (buffer nil))
    (with-minibuffer-selected-window
      (setq continue (and (derived-mode-p 'elpaca-ui-mode)
                          (member this-command '(elpaca-ui-search elpaca-ui-search-edit)))
            buffer   (current-buffer)))
    (when continue
      (add-hook 'post-command-hook (lambda () (elpaca-ui--debounce-search buffer))
                nil :local))))

(defun elpaca-ui--parse-search-filter (filter)
  "Return a list of form ((TAGS...) ((COLUMN)...)) for FILTER string."
  (let ((chars (mapcar #'string-to-char (split-string filter "" 'omit-nulls)))
        acc col cols escapedp last negatedp tagp tags)
    (dolist (char chars)
      (setq last char)
      (cond
       (escapedp (setq escapedp nil))
       ((memq char '(?\ ?|))
        (when acc (push (cons (apply #'string (nreverse acc)) negatedp) (if tagp tags col)))
        (when (eq char ?|) (push (nreverse col) cols) (setq col nil))
        (setq acc nil tagp nil char nil negatedp nil))
       ((eq char ?#)  (setq tagp t char nil))
       ((eq char ?!)  (setq negatedp t char nil))
       ((eq char ?\\) (setq escapedp t)))
      (when char (push char acc)))
    (when acc (push (cons (apply #'string (nreverse acc)) negatedp) (if tagp tags col)))
    (when col (push (if (eq last ?|) '(("")) (nreverse col)) cols))
    (list (nreverse tags) (nreverse cols))))

(defmacro elpaca-ui--query-entries (parsed)
  "Return `cl-loop' from PARSED."
  (declare (indent 1) (debug t))
  (let* ((tags (car parsed))
         (cols (cadr parsed))
         (match-all-p (= (length cols) 1))
         (columns
          (cl-loop
           for i from 0 to (length cols)
           for column = (delq nil (nth i cols))
           append
           (cl-loop for (query . negated) in column
                    for condition =
                    (when-let (((not (string-empty-p query)))
                               (target (if match-all-p
                                           '(string-join data " ")
                                         `(aref data ,i))))
                      `(string-match-p ,query ,target))
                    when (and condition negated) do (setq condition `(not ,condition))
                    when condition collect condition))))
    `(let ((filtered ,(if columns
                          `(cl-loop for entry in (funcall elpaca-ui-entries-function)
                                    for data = (cadr entry)
                                    when (and ,@columns)
                                    collect entry)
                        '(funcall elpaca-ui-entries-function))))
       ,@(when tags
           `((cl-loop for (tag . negated) in ',tags
                      for tag = (intern tag)
                      for fn = (or (alist-get tag elpaca-ui-search-tags) tag)
                      when (functionp fn)
                      do (setq filtered
                               (if negated
                                   (cl-set-difference filtered (funcall fn filtered))
                                 (funcall fn filtered))))))
       filtered)))

(defun elpaca-ui--apply-faces (buffer)
  "Update entry faces for marked, installed packages in BUFFER.
Assumes BUFFER in `elpaca-ui-mode'."
  (when elpaca-ui--want-faces
    (with-current-buffer buffer
      (cl-loop
       for (item . elpaca-or-action) in (append elpaca-ui--marked-packages (elpaca--queued))
       for markedp = (not (elpaca<-p elpaca-or-action))
       do
       (save-excursion
         (goto-char (point-min))
         (let ((continue t))
           (while (and continue (not (eobp)))
             (if-let ((package (ignore-errors (elpaca-ui-current-package)))
                      ((eq package item))
                      (start (line-beginning-position))
                      (o (if markedp
                             (make-overlay start (line-end-position))
                           (make-overlay start (+ start (length (symbol-name item)))))))
                 (let ((face   (when markedp (or (nth 2 elpaca-or-action) 'elpaca-ui-marked-package)))
                       (prefix (when markedp (or (nth 1 elpaca-or-action) "*"))))
                   (setq continue nil)
                   (when markedp
                     (overlay-put o 'before-string  (propertize (concat prefix " ") 'face face)))
                   (overlay-put o 'face (or face
                                            (elpaca--status-face
                                             (elpaca--status elpaca-or-action))))
                   (overlay-put o 'evaporate t)
                   (overlay-put o 'priority (if markedp 1 0))
                   (overlay-put o 'type 'elpaca-mark))
               (forward-line)))))))))

(defun elpaca-ui--update-search-filter (&optional buffer query)
  "Update the BUFFER to reflect search QUERY.
If QUERY is nil, the contents of the minibuffer are used instead."
  (let ((query (or query (and (minibufferp) (minibuffer-contents-no-properties)))))
    (with-current-buffer (get-buffer-create (or buffer (current-buffer)))
      (if (string-empty-p query)
          (setq tabulated-list-entries (funcall elpaca-ui-entries-function))
        (let ((parsed (elpaca-ui--parse-search-filter query)))
          (setq tabulated-list-entries (eval `(elpaca-ui--query-entries ,parsed) t)
                elpaca-ui-search-filter query)
          (tabulated-list-print 'remember-pos)
          (elpaca-ui--apply-faces buffer)
          (when elpaca-ui-header-line-function
            (setq header-line-format (funcall elpaca-ui-header-line-function
                                              parsed elpaca-ui-header-line-prefix))))))))

(defun elpaca-ui--debounce-search (buffer)
  "Update BUFFER's search filter from minibuffer."
  (let ((input (string-trim (minibuffer-contents-no-properties))))
    (unless (string= input elpaca-ui--previous-minibuffer-contents)
      (setq elpaca-ui--previous-minibuffer-contents input)
      (if elpaca-ui--search-timer
          (cancel-timer elpaca-ui--search-timer))
      (setq elpaca-ui--search-timer (run-at-time elpaca-ui-search-debounce-interval
                                                 nil
                                                 #'elpaca-ui--update-search-filter
                                                 buffer)))))

(defun elpaca-ui-search (&optional edit)
  "Filter current buffer by string.
If EDIT is non-nil, edit the last search."
  (interactive "P")
  (if edit
      (read-from-minibuffer "Search (empty to clear): " elpaca-ui-search-filter)
    (elpaca-ui--update-search-filter
     (current-buffer)
     (setq elpaca-ui-search-filter
           (let ((query (condition-case nil
                            (prog1
                                (read-from-minibuffer "Search (empty to clear): ")
                              (push elpaca-ui-search-filter elpaca-ui-search-history)
                              (when elpaca-ui--search-timer (cancel-timer elpaca-ui--search-timer)))
                          (quit elpaca-ui-search-filter))))
             (if (string-empty-p query) elpaca-ui-default-query query))))))

(defun elpaca-ui-search-refresh (&optional buffer)
  "Rerun the current search for BUFFER.
If BUFFER is non-nil, the current buffer is used."
  (interactive (list (current-buffer)))
  (elpaca-ui--update-search-filter (or buffer (current-buffer)) elpaca-ui-search-filter)
  (message "Search %S refreshed" elpaca-ui-search-filter))

(defun elpaca-ui-current-package ()
  "Return current package of UI line."
  (or (get-text-property (line-beginning-position) 'tabulated-list-id)
      (user-error "No package found at point")))

(defun elpaca-ui-browse-package ()
  "Display general info for package on current line."
  (interactive)
  (if-let ((item (elpaca-ui-current-package))
           (candidate (elpaca-alist-get item (elpaca-menu--candidates)))
           (url (plist-get candidate :url)))
      (browse-url url)
    (user-error "No URL associated with current line")))

(defun elpaca-ui-package-marked-p (package)
  "Return t if PACKAGE is marked."
  (and (member package (mapcar #'car elpaca-ui--marked-packages)) t))

(defun elpaca-ui--unmark (package)
  "Unmark PACKAGE."
  (setq-local elpaca-ui--marked-packages
              (cl-remove-if (lambda (cell) (string= (car cell) package))
                            elpaca-ui--marked-packages))
  (with-silent-modifications
    (mapc #'delete-overlay
          (cl-remove-if-not (lambda (o) (eq (overlay-get o 'type) 'elpaca-mark))
                            (overlays-at (line-beginning-position))))
    (save-restriction
      (narrow-to-region (line-beginning-position) (line-end-position))
      (elpaca-ui--apply-faces (current-buffer))))
  (forward-line))

(defun elpaca-ui-unmark ()
  "Unmark current package.
If region is active unmark all packages in region."
  (interactive)
  (if (not (use-region-p))
      (elpaca-ui--unmark (elpaca-ui-current-package))
    (save-excursion
      (save-restriction
        (narrow-to-region (save-excursion (goto-char (region-beginning))
                                          (line-beginning-position))
                          (region-end))
        (goto-char (point-min))
        (while (not (eobp))
          (condition-case _
              (progn
                (elpaca-ui--unmark (elpaca-ui-current-package)))
            ((error) (forward-line))))))))

(defun elpaca-ui-mark (package &optional action)
  "Mark PACKAGE for ACTION with PREFIX.
ACTION is the description of a cell in `elpaca-ui-actions'.
The action's function is passed the name of the package as its sole argument."
  (interactive)
  (with-silent-modifications
    (cl-pushnew (cons package (assoc action elpaca-ui-actions))
                elpaca-ui--marked-packages
                :test (lambda (a b) (string= (car a) (car b))))
    (save-restriction
      (narrow-to-region (line-beginning-position) (line-end-position))
      (elpaca-ui--apply-faces (current-buffer))))
  (forward-line))

(defun elpaca-ui--toggle-mark (&optional test action)
  "Toggle ACTION mark for current package.
TEST is a unary function evaluated prior to toggling the mark.
The current package is its sole argument."
  (if-let ((package (elpaca-ui-current-package)))
      (progn
        (when test (funcall test package))
        (if (elpaca-ui-package-marked-p package)
            (elpaca-ui--unmark package)
          (elpaca-ui-mark package action)))
    (user-error "No package associated with current line")))

(defmacro elpaca-ui-defmark (name test)
  "Define a marking command with NAME and TEST."
  (declare (indent 1) (debug t))
  `(defun ,(intern (format "elpaca-ui-mark-%s" name)) ()
     ,(format "Mark package for %s." name)
     (interactive)
     (if (not (use-region-p))
         (elpaca-ui--toggle-mark ,test ,name)
       (save-excursion
         (save-restriction
           (narrow-to-region (save-excursion (goto-char (region-beginning))
                                             (line-beginning-position))
                             (region-end))
           (goto-char (point-min))
           (while (not (eobp))
             (condition-case _
                 (elpaca-ui--toggle-mark ,test ,name)
               ((error) (forward-line)))))))))

(elpaca-ui-defmark "rebuild"
  (lambda (p) (unless (elpaca-installed-p p)
                (user-error "Package %S is not installed" p))))

(elpaca-ui-defmark "install"
  (lambda (p)
    (when (elpaca-installed-p p) (user-error "Package %S already installed" p))))

(elpaca-ui-defmark "delete"
  (lambda (p) (unless (or (elpaca-installed-p p)
                          (let ((recipe (elpaca-recipe p)))
                            (or (file-exists-p (elpaca-build-dir recipe))
                                (file-exists-p (elpaca-repo-dir recipe)))))
                (user-error "Package %S is not installed" p))))

(defvar elpaca-manager-buffer)
(defvar elpaca-status-buffer)
(defvar elpaca-status-auto-kill)
(defun elpaca-ui--post-execute-marks ()
  "Executed after `elpaca-ui-execute-marks'."
  (setq elpaca--finalize-queue-hook nil)
  (with-current-buffer elpaca-manager-buffer (elpaca-ui-search-refresh))
  (when elpaca-status-auto-kill (kill-buffer elpaca-status-buffer)))

(defun elpaca-ui-execute-marks ()
  "Execute each action in `elpaca-ui-marked-packages'."
  (interactive)
  (deactivate-mark)
  (elpaca-split-queue)
  (setq elpaca--finalize-queue-hook '(elpaca-ui--post-execute-marks))
  (when elpaca-ui--marked-packages
    (cl-loop for marked in (nreverse elpaca-ui--marked-packages)
             for action = (nth 3 (cdr marked))
             when action do
             (condition-case err
                 (funcall action (car marked))
               ((error) (message "Executing mark %S failed: %S" marked err))))
    (setq elpaca-ui--marked-packages nil)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (condition-case _
            (elpaca-ui-unmark)
          ((error) (forward-line)))))
    (elpaca-ui-search-refresh)
    (when (functionp elpaca-ui-entries-function)
      (funcall elpaca-ui-entries-function))))

(defmacro elpaca-ui-defsearch (name query)
  "Define a QUERY toggle command with NAME."
  `(defun ,(intern (format "elpaca-ui-search-%s"
                           (replace-regexp-in-string "[[:space:]]+" "-" name)))
       (toggle)
     ,(format "Search for packages which are %s.
If TOGGLE is non-nil, invert search." name)
     (interactive "P")
     (elpaca-ui--update-search-filter
      (current-buffer)
      (if toggle ,(mapconcat (lambda (token)
                               (if (string-prefix-p "!" token)
                                   (substring token 1)
                                 (concat "!" token)))
                             (split-string query " " 'omit-nulls)
                             " ")
        ,query))))

(elpaca-ui-defsearch "marked"     "#marked")
(elpaca-ui-defsearch "installed"  "#installed")
(elpaca-ui-defsearch "undeclared" "#installed !#declared")
(elpaca-ui-defsearch "orphans"    "#orphan")

(defun elpaca-ui-search-previous ()
  "Restore last search query."
  (interactive)
  (if-let ((previous (progn (pop elpaca-ui-search-history)
                            (pop elpaca-ui-search-history))))
      (elpaca-ui--update-search-filter (current-buffer) previous)
    (user-error "End of search history")))

(defun elpaca-ui-send-input ()
  "Send input string to current process."
  (interactive)
  (if-let ((p (get-text-property (line-beginning-position) 'elpaca))
           (process (elpaca<-process p))
           ((process-live-p process)))
      (let* ((input (read-string (format "Send input to %S: " (process-name process)))))
        (process-send-string process (concat input "\n")))
    (user-error "No running process associated with %S" (elpaca<-package p))))

(defun elpaca-ui--visit (type)
  "Visit current E's TYPE dir.
TYPE is either the symbol `repo` or `build`."
  (if-let ((e (get-text-property (line-beginning-position) 'elpaca))
           (dir   (funcall (intern (format "elpaca<-%s-dir" type)) e))
           ((file-exists-p dir)))
      (dired dir)
    (user-error "No %s dir associated with current line" type)))

(defun elpaca-ui-visit-repo ()
  "Visit repo associated with current package."
  (interactive)
  (elpaca-ui--visit 'repo))

(defun elpaca-ui-visit-build ()
  "Visit builds dir associated with current package."
  (interactive)
  (elpaca-ui--visit 'build))

(provide 'elpaca-ui)
;;; elpaca-ui.el ends here
