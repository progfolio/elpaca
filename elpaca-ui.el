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
  '((delete
     :prefix "💀" :face (:inherit default :weight bold :foreground "#FF0022")
     :action (lambda (i) (elpaca-delete-package 'force nil i)))
    (install
     :prefix "⚙️" :face(:inherit default :weight bold :foreground "#89cff0")
     :setup (lambda () (elpaca-log "#unique !finished"))
     :action elpaca-try-package)
    (rebuild :prefix "♻️️" :face (:inherit default :weight bold :foreground "#f28500")
             :setup (lambda () (elpaca-log
                                (if (eq (length elpaca-ui--marked-packages) 1)
                                    (format "#rebuild #linked-errors ^%s$|"
                                            (caar elpaca-ui--marked-packages))
                                  "#rebuild #unique #linked-errors")))
             :action (lambda (it) (elpaca-rebuild-package it 'hide))))
  "List of actions which can be taken on packages.
Each element is of the form: (DESCRIPTION PREFIX FACE FUNCTION)."
  :type 'list)

(defcustom elpaca-ui-search-tags
  '((dirty     . (lambda (items) (cl-remove-if-not #'elpaca-worktree-dirty-p items :key #'car)))
    (declared  . (lambda (items) (cl-remove-if-not #'elpaca-declared-p items :key #'car)))
    (orphan    . (lambda (items) (cl-remove-if-not #'elpaca-ui--orphan-p items :key #'car)))
    (unique    . (lambda (items) (cl-remove-duplicates items :key #'car :from-end t)))
    (rebuild   . elpaca-log--build-entries)
    (linked-errors . elpaca-ui--byte-comp-warnings)
    (random    . (lambda (items)
                   (if (< (length items) 10)
                       items
                     (cl-loop with (results seen)
                              until (= (length results) 10)
                              for n = (random (length items))
                              unless (memq n seen) do (push (nth n items) results)
                              (push n seen)
                              finally return results))))
    (installed . (lambda (items) (cl-remove-if-not #'elpaca-installed-p items :key #'car)))
    (marked    . (lambda (items) (cl-loop for (item . _) in elpaca-ui--marked-packages
                                          collect (assoc item items)))))
  "Alist of search tags.
Each cell is of form (NAME FILTER).
FILTER must be a unary function which takes a list of menu items and returns a
list of menu items.

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
(defun elpaca-ui--header-line (&optional prefix)
  "Set `header-line-format' to reflect query.
If PREFIX is non-nil it is displayed before the rest of the header-line."
  (setq header-line-format
        (list
         (concat
          (or prefix "")
          (propertize (format " (%d matches) " (length tabulated-list-entries))
                      'face '(:weight bold))
          " "
          elpaca-ui-search-filter))))


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

(defun elpaca-ui--fallback-date (e)
  "Return time of last modification for E's built elisp, otherwise nil."
  (file-attribute-modification-time
   (file-attributes (expand-file-name (concat (elpaca<-package e) ".el")
                                      (elpaca<-build-dir e)))))

(defun elpaca-ui--custom-candidates ()
  "Return declared candidate list with no recipe in `elpaca-menu-functions'."
  (cl-loop for (item . e) in (elpaca--queued)
           unless (elpaca-menu-item nil item nil nil 'no-descriptions)
           collect (list item :source "Init file"
                         :date (ignore-errors (elpaca-ui--fallback-date e))
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

;;TODO: clean this up.
(defun elpaca-ui--parse-search-filter (filter)
  "Return a list of form ((TYPE (QUERY NEGATED))...) for FILTER string."
  (let* ((chars (mapcar #'string-to-char (split-string (string-trim filter) "" 'omit-nulls)))
         (limit (1- (length chars)))
         (colcount -1)
         query queries col escapedp negatedp tagp skip lastp char)
    (dotimes (i (length chars))
      (setq char (nth i chars) lastp (eq i limit) skip nil)
      (cond
       (escapedp      (setq escapedp nil))
       ((and (not (zerop i)) (eq (nth (1- i) chars) ?\\)) (setq escapedp t))
       ((eq char ?\ ) (setq skip t))
       ((eq char ?|)  (setq skip t) (unless tagp (cl-incf colcount)))
       ((eq char ?#)
        (setq skip t tagp (not lastp))
        (when (and col (not lastp)) (setq colcount -1)))
       ((eq char ?!)  (setq skip t  negatedp (not lastp))))
      (unless skip (push char query))
      (when (or (not escapedp) lastp)
        (when-let ((query)
                   ((or (member char '(?\ ?|)) lastp))
                   (data (list (apply #'string (nreverse query)) negatedp)))
          (push (if tagp (push 'tag data) data) (if tagp queries col))
          (unless col (setq colcount -1))
          (setq query nil tagp nil negatedp nil))
        (when-let ((col)
                   ((or (member char '(?| ?#)) lastp)))
          (when (and lastp (>= colcount 0) (not (eq char ?|))) (cl-incf colcount))
          (push (if (eq colcount -1)
                    `(full-text ,@(nreverse col))
                  `(col ,colcount ,@(nreverse col)))
                queries)
          (when (eq char ?#) (setq colcount -1))
          (setq char nil col nil))))
    (nreverse queries)))

(defun elpaca-ui--search-fn (parsed)
  "Return query function from PARSED." ;;@TODO: Clean this up. Reptition.
  (when parsed
    (let ((body nil)
          (i 0))
      (while (< i (length parsed))
        (let* ((op (nth i parsed))
               (type (car op))
               (props (cdr op)))
          (cond
           ((eq type 'tag)
            (when-let ((fn (alist-get (intern (car props)) elpaca-ui-search-tags))
                       ((functionp fn)))
              (push (if (cadr props)
                        `(cl-set-difference entries (,fn entries))
                      `(,fn entries))
                    body)))
           ((eq type 'full-text)
            (push `(cl-loop for entry in entries
                            for data = (string-join (cadr entry) " ")
                            when (and
                                  ,@(cl-loop for (query negated) in props
                                             collect (if negated
                                                         `(not (string-match-p ,query data))
                                                       `(string-match-p ,query data))))
                            collect entry)
                  body))
           ((eq type 'col)
            (let ((cols (cl-loop for p in (nthcdr i parsed)
                                 when (eq (car p) 'col)
                                 collect (and (cl-incf i) p))))
              (cl-decf i)
              (push `(cl-loop
                      for entry in entries
                      for data = (cadr entry)
                      when (and
                            ,@(cl-loop
                               for (_ n . queries) in cols
                               append (cl-loop for (q negated) in queries
                                               collect
                                               (if negated
                                                   `(not (string-match-p ,q (aref data ,n)))
                                                 `(string-match-p ,q (aref data ,n))))))
                      collect entry)
                    body))))
          (cl-incf i)))
      `(lambda ()
         (let ((entries (funcall elpaca-ui-entries-function)))
           ,@(mapcar (lambda (form) `(setq entries ,form)) (nreverse body)))))))

(defun elpaca-ui--apply-faces (buffer)
  "Update entry faces for marked, installed packages in BUFFER.
Assumes BUFFER in `elpaca-ui-mode'."
  (with-current-buffer buffer
    (cl-loop
     for (item . elpaca-or-action) in (append elpaca-ui--marked-packages (elpaca--queued))
     for markedp = (not (elpaca<-p elpaca-or-action))
     do
     (save-excursion
       (goto-char (point-min))
       (let ((continue t))
         (while (and continue (not (eobp)))
           (if-let (((or markedp elpaca-ui--want-faces))
                    (package (ignore-errors (elpaca-ui-current-package)))
                    ((eq package item))
                    (start (line-beginning-position))
                    (o (if markedp
                           (make-overlay start (line-end-position))
                         (make-overlay start (+ start (length (symbol-name item)))))))
               (let* ((props (and markedp (cdr elpaca-or-action)))
                      (face  (and props (or (plist-get props :face) 'elpaca-ui-marked-package)))
                      (prefix (and props (or (plist-get props :prefix) "*"))))
                 (setq continue nil)
                 (when markedp
                   (overlay-put o 'before-string  (propertize (concat prefix " ") 'face face)))
                 (overlay-put o 'face (or face
                                          (elpaca--status-face
                                           (elpaca--status elpaca-or-action))))
                 (overlay-put o 'evaporate t)
                 (overlay-put o 'priority (if markedp 1 0))
                 (overlay-put o 'type 'elpaca-mark))
             (forward-line))))))))

(defun elpaca-ui--update-search-filter (&optional buffer query)
  "Update the BUFFER to reflect search QUERY.
If QUERY is nil, the contents of the minibuffer are used instead."
  (let ((query (or query (and (minibufferp) (minibuffer-contents-no-properties))
                   elpaca-ui-search-filter)))
    (with-current-buffer (get-buffer-create (or buffer (current-buffer)))
      (if (string-empty-p query)
          (setq tabulated-list-entries (funcall elpaca-ui-entries-function))
        (when-let ((parsed (elpaca-ui--parse-search-filter query))
                   (fn (elpaca-ui--search-fn parsed)))
          (setq tabulated-list-entries (funcall (byte-compile fn))
                elpaca-ui-search-filter query)
          (tabulated-list-print 'remember-pos)
          (elpaca-ui--apply-faces buffer)
          (when elpaca-ui-header-line-function
            (setq header-line-format (funcall elpaca-ui-header-line-function
                                              elpaca-ui-header-line-prefix))))))))

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

(defun elpaca-ui-search-refresh (&optional buffer silent)
  "Rerun the current search for BUFFER.
If BUFFER is non-nil, the current buffer is used.
If SILENT is non-nil, supress update message."
  (interactive (list (current-buffer)))
  (elpaca-ui--update-search-filter (or buffer (current-buffer))
                                   (or elpaca-ui-search-filter
                                       elpaca-ui-default-query))
  (unless silent (message "Search %S refreshed" elpaca-ui-search-filter)))

(defun elpaca-ui-current-package ()
  "Return current package of UI line."
  (or (get-text-property (line-beginning-position) 'tabulated-list-id)
      (user-error "No package found at point")))

(defun elpaca-ui-browse-package ()
  "Display general info for package on current line."
  (interactive)
  (if-let ((item (elpaca-ui-current-package))
           (candidate (elpaca-alist-get item (elpaca--menu-items)))
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
                elpaca-ui--marked-packages :key #'car)
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
         (elpaca-ui--toggle-mark ,test ',name)
       (save-excursion
         (save-restriction
           (narrow-to-region (save-excursion (goto-char (region-beginning))
                                             (line-beginning-position))
                             (region-end))
           (goto-char (point-min))
           (while (not (eobp))
             (condition-case _
                 (elpaca-ui--toggle-mark ,test ',name)
               ((error) (forward-line)))))))))

(elpaca-ui-defmark rebuild
  (lambda (p) (unless (elpaca-installed-p p)
                (user-error "Package %S is not installed" p))))

(elpaca-ui-defmark install
  (lambda (p)
    (when (elpaca-installed-p p) (user-error "Package %S already installed" p))))

(elpaca-ui-defmark delete
  (lambda (p) (unless (or (elpaca-installed-p p)
                          (let ((recipe (elpaca-recipe p)))
                            (or (file-exists-p (elpaca-build-dir recipe))
                                (file-exists-p (elpaca-repo-dir recipe)))))
                (user-error "Package %S is not installed" p))))

(defvar elpaca-log-auto-kill)
(defvar elpaca-manager-buffer)
(defvar elpaca-log-buffer)

;;@FIX: doesn't get run after rebuilds because the queue is already finalized.
;; do we want to pluck previously queued orders into a new queue on rebuild?
(defun elpaca-ui--post-execute-marks ()
  "Executed after `elpaca-ui-execute-marks'."
  (setq elpaca--finalize-queue-hook nil)
  (with-current-buffer elpaca-manager-buffer (elpaca-ui-search-refresh))
  (when elpaca-log-auto-kill (kill-buffer elpaca-log-buffer)))

(defun elpaca-ui-execute-marks ()
  "Execute each action in `elpaca-ui-marked-packages'."
  (interactive)
  (if (not elpaca-ui--marked-packages)
      (user-error "No packages marked")
    (deactivate-mark)
    (elpaca-split-queue)
    (setq elpaca--finalize-queue-hook '(elpaca-ui--post-execute-marks))
    (cl-loop with setups
             with actions
             for (item . (_ . props)) in  elpaca-ui--marked-packages
             for action = (plist-get props :action)
             for setup = (plist-get props :setup)
             when setup do (cl-pushnew setup setups)
             when action do
             (push (cons action item) actions)
             finally do (progn
                          (mapc #'funcall setups)
                          (mapc (lambda (a) (funcall (car a) (cdr a))) actions)))
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
(elpaca-ui-defsearch "installed"  "#installed #unique")
(elpaca-ui-defsearch "undeclared" "#installed !#declared #unique")
(elpaca-ui-defsearch "orphans"    "#orphan #unique")

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
  (if-let ((id (get-text-property (point) 'tabulated-list-id))
           (e (alist-get id (elpaca--queued)))
           (process (elpaca<-process e))
           ((process-live-p process)))
      (let* ((input (read-string (format "Send input to %S: " (process-name process)))))
        (process-send-string process (concat input "\n")))
    (user-error "No running process associated with %S" (elpaca<-package e))))

(defun elpaca-ui--visit (type)
  "Visit current E's TYPE dir.
TYPE is either the symbol `repo` or `build`."
  (if-let ((id (get-text-property (point) 'tabulated-list-id))
           (e (alist-get id (elpaca--queued)))
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

(defun elpaca-ui--visit-byte-comp-warning (file line col)
  "Visit warning location in FILE at LINE and COL."
  (or (file-exists-p file) (user-error "File does not exist: %S" file))
  (find-file-other-window file)
  (goto-char (point-min))
  (forward-line (1- line))
  (move-to-column (1- col)))

(defun elpaca-ui--byte-comp-warnings (entries)
  "Buttonize byte comp warnings in ENTRIES."
  (mapcar (lambda (entry)
            (if-let ((cols (cadr entry))
                     ((equal (aref cols 1) "byte-compilation"))
                     (copy (copy-tree entry))
                     (info (aref (cadr copy) 2))
                     (e (get-text-property (point-min) 'elpaca (aref (cadr copy) 0))))
                (progn
                  (when (string-match-p "Warning:" info)
                    (setf (aref (cadr copy) 2) (propertize info 'face 'elpaca-failed)))
                  (when (string-match "\\(?:\\([^z-a]*?\\):\\([[:digit:]]+?\\):\\([[:digit:]]+?\\)\\)" info)
                    (let ((file (match-string 1 (aref (cadr copy) 2)))
                          (line  (match-string 2 (aref (cadr copy) 2)))
                          (col (match-string 3 (aref (cadr copy) 2))))
                      (setf (aref (cadr copy) 2)
                            (replace-match
                             (buttonize (string-join (list file col line) ":")
                                        (lambda (&rest _)
                                          (elpaca-ui--visit-byte-comp-warning
                                           (expand-file-name file (elpaca<-build-dir e))
                                           (string-to-number line)
                                           (string-to-number col))))
                             nil nil (aref (cadr copy) 2)))))
                  copy)
              entry))
          entries))

(provide 'elpaca-ui)
;;; elpaca-ui.el ends here
