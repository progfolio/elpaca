;;; parcel-ui.el --- package UI for parcel.el  -*- lexical-binding: t; -*-

;;; Commentary:
;; Package search, maintenance UI.
(require 'parcel)
(require 'url)
(require 'tabulated-list)

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
  :group 'striaght-ui
  :prefix "parcel-ui-")

;;;; Customizations:
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
    ("random"    . parcel-ui-tag-random)
    ("installed" . parcel-ui-tag-installed))
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
(defvar parcel-ui--search-timer nil "Timer to debounce search input.")
(defvar parcel-ui--marked-packages nil
  "List of marked packages. Each element is a cons of (PACKAGE . ACTION).")
(defvar parcel-ui-buffer "* Parcel-UI *")
(defvar parcel-ui--entry-cache nil "Cache of all menu items.")
(defvar parcel-ui-mode-map (make-sparse-keymap) "Keymap for `parcel-ui-mode'.")
(defvar parcel-ui-package-info-mode-map (make-sparse-keymap))
(defvar parcel-ui--previous-minibuffer-contents ""
  "Keep track of minibuffer contents changes.
Allows for less debouncing than during `post-command-hook'.")
(defvar parcel-ui-search-filter nil "Filter for package searches.")
(defvar parcel-ui-search-active nil "Whether or not a search is in progress.")
(defvar parcel-ui-show-installed nil "When non-nil only show installed packages.")
(defvar parcel-ui-tag-random-chance 300)
(defvar url-http-end-of-headers)

;;;; Functions:
(defun parcel-ui--installed-p (item)
  "Return non-nil if ITEM's associated repo dir is on disk."
  (when-let ((order (parcel-alist-get item parcel--queued-orders))
             ((file-exists-p (parcel-order-repo-dir order))))
    t))

(defun parcel-ui-entries (&optional recache)
  "Return list of all entries available in `parcel-menu-functions'.
If RECACHE is non-nil, recompute `parcel-ui--entry-cache."
  (or (and (not recache) parcel-ui--entry-cache)
      (setq parcel-ui--entry-cache
            (cl-remove-duplicates
             (cl-loop for (item . data) in (parcel-menu--candidates)
                      when item
                      collect (list
                               item
                               (vector (format "%S" item)
                                       (or (plist-get data :description) "")
                                       (or (plist-get data :source) ""))))
             :key #'car :from-end t))))

(defun parcel--ui-init ()
  "Initialize format of the UI."
  (setq tabulated-list-format
        [("Package" 30 t)
         ("Description" 80 t)
         ("Source" 20 t)]
        tabulated-list-entries
        #'parcel-ui-entries))

(defun parcel-ui-show-installed ()
  "Show only installed packages.
Toggle all if already filtered."
  (interactive)
  (setq parcel-ui-search-filter
        (if (setq parcel-ui-show-installed (not parcel-ui-show-installed))
            ".*"
          "#installed"))
  (parcel-ui--update-search-filter parcel-ui-search-filter))

(define-derived-mode parcel-ui-mode tabulated-list-mode "parcel-ui"
  "Major mode to manage packages."
  (parcel--ui-init)
  (setq tabulated-list-use-header-line nil)
  (tabulated-list-init-header)
  (tabulated-list-print 'remember-pos 'update)
  (add-hook 'minibuffer-setup-hook 'parcel-ui--minibuffer-setup))

;;;###autoload
(defun parcel-ui ()
  "Display the UI."
  (interactive)
  (with-current-buffer (get-buffer-create parcel-ui-buffer)
    (unless (derived-mode-p 'parcel-ui-mode)
      (parcel-ui-mode))
    (pop-to-buffer parcel-ui-buffer)))

(defun parcel-ui--minibuffer-setup ()
  "Set up the minibuffer for live filtering."
  (when parcel-ui-search-active
    (add-hook 'post-command-hook
              'parcel-ui--debounce-search nil :local)))

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
     ;;ignore negation operator by itself
     ((string-empty-p query) t)
     (negated (not match))
     (t match))))

;; @TODO Implement these:
;; (defun parcel--package-on-default-branch-p (package)
;; (defun parcel-ui--local-branch-behind-p (package)

(defun parcel-ui--worktree-dirty-p (item)
  "Return t if ITEM has a dirty worktree."
  (when-let ((order (parcel-alist-get item parcel--queued-orders))
             (recipe (parcel-order-recipe order))
             (repo-dir (parcel-order-repo-dir order))
             ((file-exists-p repo-dir)))
    (let ((default-directory repo-dir))
      (not (string-empty-p
            (parcel-process-output "git" "-c" "status.branch=false"
                                   "status" "--short"))))))

(defun parcel-ui-tag-dirty (candidate)
  "Return t if CANDIDATE's worktree is ditry."
  (parcel-ui--worktree-dirty-p (car candidate)))

(defun parcel-ui-tag-declared (candidate)
  "Return t if CANDIDATE declared in init or an init declaration dependency."
  (when-let ((item (car candidate))
             (order (parcel-alist-get item parcel--queued-orders)))
    (or (parcel-order-init order)
        (cl-some #'parcel-order-init
                 (delq nil
                       (mapcar (lambda (dependent)
                                 (parcel-alist-get dependent parcel--queued-orders))
                               (parcel-dependents item)))))))

(defun parcel-ui-tag-installed (candidate)
  "Return t if CANDIDATE is installed."
  (parcel-ui--installed-p (car candidate)))

(defun parcel-ui-tag-random (_)
  "1/1000th of a chance candidate is shown."
  (zerop (random parcel-ui-tag-random-chance)))

;;@MAYBE: allow literal string searches?
;;similar to a macro, a tag expands to a search?
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
    `(cl-loop for entry in (parcel-ui-entries)
              ,@(when columns '(for metadata = (cadr entry)))
              ,@(apply #'append columns)
              ,@(when tags (apply #'append tags))
              collect entry)))

(defun parcel-ui--search-header (parsed)
  "Set `header-line-format' to reflect PARSED query."
  (let* ((tags (car parsed))
         (cols (cadr parsed))
         (full-match-p (= (length cols) 1)))
    (setq header-line-format
          (concat (propertize (format "Parcel Query (%d packages):"
                                      (length tabulated-list-entries))
                              'face '(:weight bold))
                  " "
                  (when (and cols (not (equal cols '(nil))))
                    (concat
                     (propertize
                      (if full-match-p "Matching:" "Columns Matching:")
                      'face 'parcel-failed)
                     " "
                     (if full-match-p
                         (string-join (car cols) ", ")
                       (mapconcat (lambda (col) (format "%s" (or col "(*)")))
                                  cols
                                  ", "))))
                  (when tags
                    (concat " " (propertize "Tags:" 'face 'parcel-failed) " "
                            (string-join
                             (mapcar
                              (lambda (tag)
                                (concat (if (string-prefix-p "#" tag) "!" "#") tag))
                              tags)
                             ", ")))))))

;;@TODO: decompose these loops into a function
;; They do essentially the same thing.
(defun parcel-ui--apply-faces ()
  "Update entry faces for marked, installed packages."
  (when-let ((buffer parcel-ui-buffer))
    (with-current-buffer buffer
      (cl-loop for (item . order) in parcel--queued-orders
               unless (assq item parcel-ui--marked-packages)
               do (when-let ((repo-dir (parcel-order-repo-dir order))
                             (file-exist-p repo-dir))
                    (save-excursion
                      (goto-char (point-min))
                      (let ((continue t))
                        (while (and continue (not (eobp)))
                          (if-let ((package (ignore-errors (parcel-ui-current-package)))
                                   ((eq package item))
                                   (start (line-beginning-position))
                                   (o (make-overlay start
                                                    (+ start (length (symbol-name item))))))
                              (progn
                                (setq continue nil)
                                (overlay-put o 'face 'parcel-finished)
                                (overlay-put o 'evaporate t)
                                (overlay-put o 'priority 0)
                                (overlay-put o 'type 'parcel-mark))
                            (forward-line)))))))
      (cl-loop for (item . spec) in parcel-ui--marked-packages
               do (save-excursion
                    (goto-char (point-min))
                    (let ((continue t))
                      (while (and continue (not (eobp)))
                        (if-let ((package (ignore-errors (parcel-ui-current-package)))
                                 ((eq package item)))
                            (let* ((o      (make-overlay (line-beginning-position) (line-end-position)))
                                   (face   (or (nth 2 spec) 'parcel-ui-marked-package))
                                   (prefix (or (nth 1 spec) "*")))
                              (setq continue nil)
                              (overlay-put o 'before-string  (propertize (concat prefix " ") 'face face))
                              (overlay-put o 'face face)
                              (overlay-put o 'evaporate t)
                              (overlay-put o 'priority 1)
                              (overlay-put o 'type 'parcel-mark))
                          (forward-line)))))))))

(defun parcel-ui--update-search-filter (&optional query)
  "Update the UI to reflect search input.
If QUERY is non-nil, use that instead of the minibuffer."
  (when-let ((buffer parcel-ui-buffer)
             (query (or query (and (minibufferp) (minibuffer-contents-no-properties)))))
    (unless (string-empty-p query)
      (with-current-buffer (get-buffer-create buffer)
        (let ((parsed (parcel-ui--parse-search-filter query)))
          (setq tabulated-list-entries (eval `(parcel-ui-query-loop ,parsed) t))
          (tabulated-list-print 'remember-pos 'update)
          (parcel-ui--apply-faces)
          (parcel-ui--search-header parsed))))))

(defun parcel-ui--debounce-search ()
  "Update filter from minibuffer."
  (let ((input (string-trim (minibuffer-contents-no-properties))))
    (unless (string= input parcel-ui--previous-minibuffer-contents)
      (setq parcel-ui--previous-minibuffer-contents input)
      (if parcel-ui--search-timer
          (cancel-timer parcel-ui--search-timer))
      (setq parcel-ui--search-timer
            (run-at-time parcel-ui-search-debounce-interval
                         nil
                         #'parcel-ui--update-search-filter)))))

(defun parcel-ui-search (&optional edit)
  "Filter current buffer by string.
If EDIT is non-nil, edit the last search."
  (interactive)
  (unwind-protect
      (setq parcel-ui-search-active t
            parcel-ui-search-filter
            (condition-case nil
                (read-from-minibuffer "Search (empty to clear): "
                                      (when edit parcel-ui-search-filter))
              (quit parcel-ui-search-filter)))
    (setq parcel-ui-search-active nil)
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
  (parcel-ui--update-search-filter parcel-ui-search-filter))

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

(defun parcel-ui-unmark (package)
  "Unmark PACKAGE."
  (interactive (list (parcel-ui-current-package)))
  (unless (equal (buffer-name) parcel-ui-buffer)
    (user-error "Can't unmark package outside of %S" parcel-status-buffer))
  (setq parcel-ui--marked-packages
        (cl-remove-if (lambda (cell) (string= (car cell) package))
                      parcel-ui--marked-packages))
  (with-silent-modifications
    (mapc #'delete-overlay
          (cl-remove-if-not (lambda (o) (eq (overlay-get o 'type) 'parcel-mark))
                            (overlays-at (line-beginning-position))))
    (save-restriction
      (narrow-to-region (line-beginning-position) (line-end-position))
      (parcel-ui--apply-faces)))
  (forward-line))

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
      (parcel-ui--apply-faces)))
  (forward-line))

(defun parcel-ui-toggle-mark (&optional test action)
  "Toggle ACTION mark for current package.
TEST is a unary function evaluated prior to toggling the mark.
The current package is its sole argument."
  (interactive)
  (unless (string= (buffer-name) parcel-ui-buffer)
    (user-error "Cannot mark outside of %S" parcel-ui-buffer))
  (if-let ((package (parcel-ui-current-package)))
      (progn
        (when test (funcall test package))
        (if (parcel-ui-package-marked-p package)
            (parcel-ui-unmark package)
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
  (lambda (p) (unless (parcel-ui--installed-p p)
                (user-error "Package %S is not installed" p))))

(parcel-ui-defmark "install"
  (lambda (p)
    (when (parcel-ui--installed-p p) (user-error "Package %S already installed" p))))

(parcel-ui-defmark "delete"
  (lambda (p) (unless (parcel-ui--installed-p p)
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
      (setq parcel-ui--marked-packages nil)
      (when-let ((buffer (get-buffer parcel-ui-buffer)))
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              (condition-case _
                  (parcel-ui-unmark (parcel-ui-current-package))
                ((error) (forward-line)))))))
      (parcel-ui-entries 'recache)
      (parcel-ui-search-refresh))))

;;;; Key bindings
(define-key parcel-ui-mode-map (kbd "*")   'parcel-ui-toggle-mark)
(define-key parcel-ui-mode-map (kbd "F")   'parcel-ui-toggle-follow-mode)
(define-key parcel-ui-mode-map (kbd "I")   'parcel-ui-show-installed)
(define-key parcel-ui-mode-map (kbd "R")   'parcel-ui-search-refresh)
(define-key parcel-ui-mode-map (kbd "RET") 'parcel-ui-describe-package)
(define-key parcel-ui-mode-map (kbd "S")   'parcel-ui-search-edit)
(define-key parcel-ui-mode-map (kbd "b")   'parcel-ui-browse-package)
(define-key parcel-ui-mode-map (kbd "d")   'parcel-ui-mark-delete)
(define-key parcel-ui-mode-map (kbd "i")   'parcel-ui-mark-install)
(define-key parcel-ui-mode-map (kbd "r")   'parcel-ui-mark-rebuild)
(define-key parcel-ui-mode-map (kbd "s")   'parcel-ui-search)
(define-key parcel-ui-mode-map (kbd "x")   'parcel-ui-execute-marks)

(provide 'parcel-ui)
;;; parcel-ui.el ends here
