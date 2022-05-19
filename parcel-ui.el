;;; parcel-ui.el --- package UI for parcel.el  -*- lexical-binding: t; -*-

;;; Commentary:
;; Package search, maintenance UI.
(require 'parcel)
(require 'url)
(require 'tabulated-list)
;;@TODO: do these need to be unconditionally required/executed?
(require 'bookmark)
(bookmark-maybe-load-default-file)


;;; Code:
;;;;Faces:
(defface parcel-ui-package
  '((default :inherit default))
  "Default face for packages."
  :group 'parcel-faces)

(defface parcel-ui-marked-package
  '((default (:inherit default :weight bold :foreground "pink")))
  "Face for marked packages."
  :group 'parcel-faces)

(defgroup parcel-ui nil
  "Parcel's UI options."
  :group 'parcel-ui
  :prefix "parcel-ui-")

;;;; Customizations:
(defcustom parcel-ui-initial-query ".*"
  "Search query used when enabling `parcel-ui-mode'."
  :type 'string)

(defcustom parcel-ui-actions
  '(("delete"  "💀" (:inherit default :weight bold :foreground "#FF0022")
     (lambda (i) (parcel-delete-package 'force nil i)))
    ("install" "⚙️" (:inherit default :weight bold :foreground "#89cff0") parcel-try-package)
    ("rebuild" "♻️️" (:inherit default :weight bold :foreground "#f28500") parcel-rebuild-package))
  "List of actions which can be taken on packages.
Each element is of the form: (DESCRIPTION PREFIX FACE FUNCTION)."
  :type 'list)

(defcustom parcel-ui-search-tags
  '(("dirty"     . parcel-ui-tag-dirty)
    ("declared"  . parcel-ui-tag-declared)
    ("orphan"    . parcel-ui-tag-orphan)
    ("random"    . parcel-ui-tag-random)
    ("installed" . parcel-ui-tag-installed)
    ("marked"    . parcel-ui-tag-marked))
  "Alist of search tags.
Each cell is of form (NAME FILTER).
If FILTER is a function it must accept a single candidate as its sole
argument and return non-nil if the package is to be kept.

Each tag can be inverted in the minibuffer by prepending an
exclamation point to it. e.g. #!installed."
  :group 'parcel-ui
  :type 'alist)

(defcustom parcel-ui-search-debounce-interval 0.25
  "Length of time to wait before updating the search UI.
See `run-at-time' for acceptable values."
  :group 'parcel-ui
  :type (or 'string 'int 'float))

;;;; Variables:
(defvar-local parcel-ui--search-timer nil "Timer to debounce search input.")
(defvar-local parcel-ui--entry-cache nil "Cache of all menu items.")
(defvar-local parcel-ui--marked-packages nil
  "List of marked packages. Each element is a cons of (PACKAGE . ACTION).")
(defvar parcel-ui--entry-cache nil "Cache of all menu items.")
(defvar parcel-ui-mode-map (make-sparse-keymap) "Keymap for `parcel-ui-mode'.")
(defvar-local parcel-ui--previous-minibuffer-contents ""
  "Keep track of minibuffer contents changes.
Allows for less debouncing than during `post-command-hook'.")
(defvar-local parcel-ui-search-filter nil "Filter for package searches.")
(defvar-local parcel-ui-search-history nil "List of previous search queries.")
(defvar-local parcel-ui-header-line-function nil
  "Function responsible for setting the UI buffer's `header-line-format'.
It recieves one argument, the parsed search query list.")
(defvar-local parcel-ui-entries-function nil
  "Function responsible for returning the UI buffer's `tabulated-list-entries'.
It takes no arguments.")


(defvar url-http-end-of-headers)

;;;; Functions:
(defun parcel-ui--orphan-p (item)
  "Return non-nil if ITEM's repo or build are on disk without having been queued."
  (let ((queued-orders (parcel--queued-orders)))
    (unless (alist-get item queued-orders)
      (let* ((recipe (parcel-recipe item))
             (repo   (parcel-repo-dir recipe)))
        (unless (cl-some (lambda (cell)
                           (when-let ((queued (cdr cell))
                                      ((equal repo (parcel-order-repo-dir queued))))
                             queued))
                         queued-orders)
          (or (file-exists-p (parcel-build-dir recipe))
              (file-exists-p (parcel-repo-dir  recipe))))))))

(defun parcel-ui-tag-orphan (candidate)
  "Return non-nil if CANDIDATE is an oprhaned package."
  (parcel-ui--orphan-p (car candidate)))

(defun parcel-ui--custom-candidates ()
  "Return declared candidate list with no recipe in `parcel-menu-functions'."
  (cl-loop for (item . order) in (parcel--queued-orders)
           unless (parcel-menu-item nil item nil nil 'no-descriptions)
           collect (list item :source "init file" :description "Not available in menu functions")))

(define-derived-mode parcel-ui-mode tabulated-list-mode "parcel-ui"
  "Major mode to manage packages."
  (parcel-ui--update-search-filter (or parcel-ui-initial-query ".*"))
  (add-hook 'minibuffer-setup-hook 'parcel-ui--minibuffer-setup))

(defun parcel-ui--minibuffer-setup ()
  "Set up the minibuffer for live filtering."
  (let ((continue nil)
        (buffer nil))
    (with-minibuffer-selected-window
      (setq continue (and (derived-mode-p 'parcel-ui-mode)
                          (member this-command '(parcel-ui-search parcel-ui-search-edit)))
            buffer   (current-buffer)))
    (when continue
      (add-hook 'post-command-hook (lambda () (parcel-ui--debounce-search buffer))
                nil :local))))

(defun parcel-ui--parse-search-filter (filter)
  "Return a list of form ((TAGS...) ((COLUMN)...)) for FILTER string."
  (let* ((tags (cl-remove-if-not (lambda (s)
                                   (string-match-p "^!?#" s))
                                 (split-string filter " " 'omit-nulls)))
         (columns (mapcar (lambda (col)
                            (cl-remove-if
                             (lambda (word) (member word tags))
                             (split-string (string-trim col) " " 'omit-nulls)))
                          (split-string filter "|"))))
    (list (mapcar (lambda (s) (substring s 1)) tags) columns)))

(defun parcel-ui--query-matches-p (query subject)
  "Return t if QUERY (negated or otherwise) agrees with SUBJECT."
  (let* ((negated (and (string-prefix-p "!" query)
                       (setq query (substring query 1))))
         (match (ignore-errors (string-match-p query subject))))
    (cond
     ;; Ignore negation operator by itself.
     ((string-empty-p query) t)
     (negated (not match))
     (t match))))

(defun parcel-ui-tag-marked (candidate)
  "Return non-nil if CANDIDATE is a marked package."
  (cl-member (car candidate) parcel-ui--marked-packages :key #'car))

(defun parcel-ui-tag-dirty (candidate)
  "Return t if CANDIDATE's worktree is ditry."
  (parcel-worktree-dirty-p (car candidate)))

(defun parcel-ui-tag-declared (candidate)
  "Return t if CANDIDATE declared in init or an init declaration dependency."
  (when-let ((item (car candidate)))
    (parcel-declared-p item)))

(defun parcel-ui-tag-installed (candidate)
  "Return t if CANDIDATE is installed."
  (parcel-installed-p (car candidate)))

(defmacro parcel-ui-query-loop (parsed)
  "Return `cl-loop' from PARSED query."
  (declare (indent 1) (debug t))
  ;;@FIX: our query parser shouldn't return junk that we have to remove here...
  (let ((tags (cl-loop for tag in
                       (cl-remove-if
                        (lambda (s) (or (string-empty-p s)
                                        (member s '("#" "!"))))
                        (car parsed))
                       for negated = (string-prefix-p "#" tag)
                       when negated do (setq tag (substring tag 1))
                       for condition = (if negated 'unless 'when)
                       for filter = (alist-get tag parcel-ui-search-tags
                                               nil nil #'string=)
                       when filter
                       collect `(,condition (,filter entry))))
        (columns
         (apply
          #'append
          (let* ((cols (cadr parsed))
                 (match-all-p (= (length cols) 1)))
            (cl-loop for i from 0 to (length cols)
                     for column = (delq nil (nth i cols))
                     collect
                     (cl-loop for query in column
                              for negated = (string-prefix-p "!" query)
                              for condition = (if negated 'unless 'when)
                              when negated do (setq query (substring query 1))
                              for target = (if match-all-p '(string-join metadata)
                                             `(aref metadata ,i))
                              unless (string-empty-p query)
                              collect
                              `(,condition (string-match-p ,query ,target))))))))
    `(cl-loop for entry in (funcall parcel-ui-entries-function)
              ,@(when columns '(for metadata = (cadr entry)))
              ,@(apply #'append columns)
              ,@(when tags (apply #'append tags))
              collect entry)))

(defun parcel-ui--apply-faces (buffer)
  "Update entry faces for marked, installed packages in BUFFER.
Assumes BUFFER in `parcel-ui-mode'."
  (with-current-buffer buffer
    (cl-loop
     for (item . order-or-action) in (append parcel-ui--marked-packages (parcel--queued-orders))
     for markedp = (not (parcel-order-p order-or-action))
     do
     (save-excursion
       (goto-char (point-min))
       (let ((continue t))
         (while (and continue (not (eobp)))
           (if-let ((package (ignore-errors (parcel-ui-current-package)))
                    ((eq package item))
                    (start (line-beginning-position))
                    (o (if markedp
                           (make-overlay start (line-end-position))
                         (make-overlay start (+ start (length (symbol-name item)))))))
               (let ((face   (when markedp (or (nth 2 order-or-action) 'parcel-ui-marked-package)))
                     (prefix (when markedp (or (nth 1 order-or-action) "*"))))
                 (setq continue nil)
                 (when markedp
                   (overlay-put o 'before-string  (propertize (concat prefix " ") 'face face)))
                 (overlay-put o 'face (or face 'parcel-finished))
                 (overlay-put o 'evaporate t)
                 (overlay-put o 'priority (if markedp 1 0))
                 (overlay-put o 'type 'parcel-mark))
             (forward-line))))))))

(defun parcel-ui--update-search-filter (buffer &optional query)
  "Update the BUFFER to reflect search QUERY.
If QUERY is nil, the contents of the minibuffer are used instead."
  (when-let ((query (or query (and (minibufferp) (minibuffer-contents-no-properties)))))
    (unless (string-empty-p query)
      (with-current-buffer (get-buffer-create buffer)
        (let ((parsed (parcel-ui--parse-search-filter query)))
          (push parcel-ui-search-filter parcel-ui-search-history)
          (setq tabulated-list-entries (eval `(parcel-ui-query-loop ,parsed) t)
                parcel-ui-search-filter query)
          (tabulated-list-print 'remember-pos)
          (parcel-ui--apply-faces buffer)
          (when parcel-ui-header-line-function
            (setq header-line-format (funcall parcel-ui-header-line-function parsed))))))))

(defun parcel-ui--debounce-search (buffer)
  "Update BUFFER's search filter from minibuffer."
  (let ((input (string-trim (minibuffer-contents-no-properties))))
    (unless (string= input parcel-ui--previous-minibuffer-contents)
      (setq parcel-ui--previous-minibuffer-contents input)
      (if parcel-ui--search-timer
          (cancel-timer parcel-ui--search-timer))
      (setq parcel-ui--search-timer
            (run-at-time parcel-ui-search-debounce-interval
                         nil
                         #'parcel-ui--update-search-filter
                         buffer)))))

(defun parcel-ui-search (&optional edit)
  "Filter current buffer by string.
If EDIT is non-nil, edit the last search."
  (interactive)
  (unwind-protect
      (setq parcel-ui-search-filter
            (condition-case nil
                (read-from-minibuffer "Search (empty to clear): "
                                      (when edit parcel-ui-search-filter))
              (quit parcel-ui-search-filter)))
    (when (string-empty-p parcel-ui-search-filter)
      ;;reset to default view
      (parcel-ui--update-search-filter ".*"))))

(defun parcel-ui-search-edit ()
  "Edit last search."
  (interactive)
  (parcel-ui-search 'edit))

(defun parcel-ui-search-refresh ()
  "Rerun the current search."
  (interactive)
  (parcel-ui--update-search-filter parcel-ui-search-filter)
  (message "Search %S refreshed" parcel-ui-search-filter))

(defun parcel-ui-current-package ()
  "Return current package of UI line."
  (or (get-text-property (line-beginning-position) 'tabulated-list-id)
      (user-error "No package found at point")))

(defun parcel-ui-browse-package ()
  "Display general info for package on current line."
  (interactive)
  (if-let ((item (parcel-ui-current-package))
           (candidate (parcel-alist-get item (parcel-menu--candidates)))
           (url (plist-get candidate :url)))
      (browse-url url)
    (user-error "No URL associated with current line")))

(defun parcel-ui-package-marked-p (package)
  "Return t if PACKAGE is marked."
  (and (member package (mapcar #'car parcel-ui--marked-packages)) t))

(defun parcel-ui--unmark (package)
  "Unmark PACKAGE."
  (setq-local parcel-ui--marked-packages
              (cl-remove-if (lambda (cell) (string= (car cell) package))
                            parcel-ui--marked-packages))
  (with-silent-modifications
    (mapc #'delete-overlay
          (cl-remove-if-not (lambda (o) (eq (overlay-get o 'type) 'parcel-mark))
                            (overlays-at (line-beginning-position))))
    (save-restriction
      (narrow-to-region (line-beginning-position) (line-end-position))
      (parcel-ui--apply-faces (current-buffer))))
  (forward-line))

(defun parcel-ui-unmark ()
  "Unmark current package.
If region is active unmark all packages in region."
  (interactive)
  (if (not (use-region-p))
      (parcel-ui--unmark (parcel-ui-current-package))
    (save-excursion
      (save-restriction
        (narrow-to-region (save-excursion (goto-char (region-beginning))
                                          (line-beginning-position))
                          (region-end))
        (goto-char (point-min))
        (while (not (eobp))
          (condition-case _
              (progn
                (parcel-ui--unmark (parcel-ui-current-package)))
            ((error) (forward-line))))))))

(defun parcel-ui-mark (package &optional action)
  "Mark PACKAGE for ACTION with PREFIX.
ACTION is the description of a cell in `parcel-ui-actions'.
The action's function is passed the name of the package as its sole argument."
  (interactive)
  (with-silent-modifications
    (cl-pushnew (cons package (assoc action parcel-ui-actions))
                parcel-ui--marked-packages
                :test (lambda (a b) (string= (car a) (car b))))
    (save-restriction
      (narrow-to-region (line-beginning-position) (line-end-position))
      (parcel-ui--apply-faces (current-buffer))))
  (forward-line))

(defun parcel-ui-toggle-mark (&optional test action)
  "Toggle ACTION mark for current package.
TEST is a unary function evaluated prior to toggling the mark.
The current package is its sole argument."
  (interactive)
  (if-let ((package (parcel-ui-current-package)))
      (progn
        (when test (funcall test package))
        (if (parcel-ui-package-marked-p package)
            (parcel-ui--unmark package)
          (parcel-ui-mark package action)))
    (user-error "No package associated with current line")))

(defmacro parcel-ui-defmark (name test)
  "Define a marking command with NAME and TEST."
  (declare (indent 1) (debug t))
  `(defun ,(intern (format "parcel-ui-mark-%s" name)) ()
     ,(format "Mark package for %s." name)
     (interactive)
     (if (not (use-region-p))
         (parcel-ui-toggle-mark ,test ,name)
       (save-excursion
         (save-restriction
           (narrow-to-region (save-excursion (goto-char (region-beginning))
                                             (line-beginning-position))
                             (region-end))
           (goto-char (point-min))
           (while (not (eobp))
             (condition-case _
                 (parcel-ui-toggle-mark ,test ,name)
               ((error) (forward-line)))))))))

(parcel-ui-defmark "rebuild"
  (lambda (p) (unless (parcel-installed-p p)
                (user-error "Package %S is not installed" p))))

(parcel-ui-defmark "install"
  (lambda (p)
    (when (parcel-installed-p p) (user-error "Package %S already installed" p))))

(parcel-ui-defmark "delete"
  (lambda (p) (unless (or (parcel-installed-p p)
                          (let ((recipe (parcel-recipe p)))
                            (or (file-exists-p (parcel-build-dir recipe))
                                (file-exists-p (parcel-repo-dir recipe)))))
                (user-error "Package %S is not installed" p))))

(defun parcel-ui--choose-deferred-action ()
  "Choose a deferred action from `parcel-ui-deferred-actions'."
  (when-let ((choice (completing-read "Action for deferred marks: "
                                      parcel-ui-actions nil 'require-match)))
    (alist-get choice parcel-ui-actions nil nil #'equal)))

(defun parcel-ui-execute-marks ()
  "Execute each action in `parcel-ui-marked-packages'."
  (interactive)
  (when parcel-ui--marked-packages
    (let ((generics nil))
      (mapc (lambda (marked)
              (if-let ((action (nth 3 (cdr marked))))
                  (condition-case err
                      (funcall action (car marked))
                    ((error) (message "Executing mark %S failed: %S" marked err)))
                (push (car marked) generics)))
            (nreverse parcel-ui--marked-packages))
      (when-let ((generics)
                 (action (nth 3 (parcel-ui--choose-deferred-action))))
        (mapc action generics))
      (setq-local parcel-ui--marked-packages nil)
      (save-excursion
        (goto-char (point-min))
        (while (not (eobp))
          (condition-case _
              (parcel-ui-unmark)
            ((error) (forward-line)))))
      (when (functionp parcel-ui-entries-function)
        (funcall parcel-ui-entries-function 'recache))
      (parcel-ui-search-refresh))))

(defmacro parcel-ui-defsearch (name query)
  "Define a QUERY toggle command with NAME."
  `(defun ,(intern (format "parcel-ui-search-%s"
                           (replace-regexp-in-string "[[:space:]]+" "-" name)))
       (toggle)
     ,(format "Search for packages which are %s.
If TOGGLE is non-nil, invert search." name)
     (interactive "P")
     (parcel-ui--update-search-filter
      (if toggle ,(mapconcat (lambda (token)
                               (if (string-prefix-p "!" token)
                                   (substring token 1)
                                 (concat "!" token)))
                             (split-string query " " 'omit-nulls)
                             " ")
        ,query))))

(parcel-ui-defsearch "marked"     "#marked")
(parcel-ui-defsearch "installed"  "#installed")
(parcel-ui-defsearch "undeclared" "#installed !#declared")
(parcel-ui-defsearch "orphans"    "#orphan")

(defun parcel-ui-search-previous ()
  "Restore last search query."
  (interactive)
  (if-let ((previous (pop parcel-ui-search-history)))
      (parcel-ui--update-search-filter previous)
    (user-error "End of search history")))

;;;; Key bindings
(define-key parcel-ui-mode-map (kbd "*")   'parcel-ui-toggle-mark)
(define-key parcel-ui-mode-map (kbd "F")   'parcel-ui-toggle-follow-mode)
(define-key parcel-ui-mode-map (kbd "I")   'parcel-ui-search-installed)
(define-key parcel-ui-mode-map (kbd "M")   'parcel-ui-search-marked)
(define-key parcel-ui-mode-map (kbd "O")   'parcel-ui-search-orphans)
(define-key parcel-ui-mode-map (kbd "P")   'parcel-ui-search-previous)
(define-key parcel-ui-mode-map (kbd "R")   'parcel-ui-search-refresh)
(define-key parcel-ui-mode-map (kbd "U")   'parcel-ui-search-undeclared)
(define-key parcel-ui-mode-map (kbd "RET") 'parcel-ui-describe-package)
(define-key parcel-ui-mode-map (kbd "S")   'parcel-ui-search-edit)
(define-key parcel-ui-mode-map (kbd "b")   'parcel-ui-browse-package)
(define-key parcel-ui-mode-map (kbd "d")   'parcel-ui-mark-delete)
(define-key parcel-ui-mode-map (kbd "i")   'parcel-ui-mark-install)
(define-key parcel-ui-mode-map (kbd "r")   'parcel-ui-mark-rebuild)
(define-key parcel-ui-mode-map (kbd "s")   'parcel-ui-search)
(define-key parcel-ui-mode-map (kbd "u")   'parcel-ui-unmark)
(define-key parcel-ui-mode-map (kbd "x")   'parcel-ui-execute-marks)

(provide 'parcel-ui)
;;; parcel-ui.el ends here
