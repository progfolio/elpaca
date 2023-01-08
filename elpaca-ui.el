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
(defcustom elpaca-ui-default-query ".*" "Initial `elpaca-ui-mode' search query."
  :type 'string)
(make-variable-buffer-local 'elpaca-ui-default-query)

(defcustom elpaca-ui-actions
  '((delete  :prefix "💀" :face (:inherit default :weight bold :foreground "#FF0022")
             :action (lambda (i) (elpaca-delete i 'force 'deps)))
    (install :prefix "⚙️" :face (:inherit default :weight bold :foreground "#89cff0")
             :setup (lambda ()
                      (elpaca-split-queue)
                      (require 'elpaca-log) (elpaca-log--latest))
             :action elpaca-try)
    (rebuild :prefix "♻️️" :face (:inherit default :weight bold :foreground "#f28500")
             :setup (lambda () (require 'elpaca-log) (elpaca-log--latest))
             :action elpaca-rebuild)
    (update  :prefix "⬆️" ️️:face (:inherit default :weight bold :foreground "#f28500")
             :setup (lambda () (require 'elpaca-log) (elpaca-log--latest))
             :action elpaca-update))
  "List of actions which can be taken on packages."
  :type 'list)

(defcustom elpaca-ui-search-tags
  '((dirty     . (lambda (items) (cl-remove-if-not #'elpaca-worktree-dirty-p items :key #'car)))
    (declared  . (lambda (items) (cl-remove-if-not #'elpaca-declared-p items :key #'car)))
    (orphan    . (lambda (items) (mapcar
                                  (lambda (dir)
                                    (let ((name (file-name-base dir)))
                                      (list (intern name)
                                            (vector (propertize name 'orphan-dir dir)
                                                    "orphan package" "n/a" "n/a" "n/a"))))
                                  (cl-set-difference
                                   (cl-remove-if-not
                                    #'file-directory-p
                                    (nthcdr 2 (directory-files elpaca-builds-directory t)))
                                   (mapcar (lambda (q) (elpaca<-build-dir (cdr q))) (elpaca--queued))
                                   :test #'equal))))
    (unique    . (lambda (items) (cl-remove-duplicates items :key #'car :from-end t)))
    (random    . (lambda (items &optional limit)
                   (if (< (length items) (or limit 10))
                       items
                     (cl-loop with (results seen)
                              until (= (length results) (or limit 10))
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
  :type 'alist)

(defcustom elpaca-ui-search-debounce-interval 0.25
  "Length of time in seconds to wait before updating the search UI."
  :type (or 'string 'int 'float))

(defun elpaca-defsearch (name query)
  "Return seach command with NAME for QUERY."
  (eval `(defun ,(intern (format "elpaca-ui-search-%s" name)) ()
           ,(format "Search for %S" query)
           (interactive)
           (elpaca-ui-search ,query))
        t))

(defun elpaca-ui--button-noop (&rest args)
  "Return first arg in ARGS."
  (car args))

(defalias 'elpaca-ui--buttonize
  (with-no-warnings (cond
                     ;;API for Emacs 27 requires allocating temp buffers. Not worth it.
                     ((version< emacs-version "28.1") #'elpaca-ui--button-noop)
                     ((version< emacs-version "29") #'button-buttonize)
                     (t #'buttonize))))

;;;; Variables:
(defvar-local elpaca-ui--search-timer nil "Timer to debounce search input.")
(defvar-local elpaca-ui--marked-packages nil
  "List of marked packages. Each element is a cons of (PACKAGE . ACTION).")
(defvar-local elpaca-ui--prev-entry-count nil "Number of previously recored entries.")
(defvar elpaca-ui-mode-map (let ((m (make-sparse-keymap)))
                             (define-key m (kbd "!") 'elpaca-ui-send-input)
                             (define-key m (kbd "I") (elpaca-defsearch 'installed "#unique #installed"))
                             (define-key m (kbd "M") (elpaca-defsearch 'marked   "#unique #marked"))
                             (define-key m (kbd "O") (elpaca-defsearch 'orphaned "#unique #orphan"))
                             (define-key m (kbd "R") 'elpaca-ui-search-refresh)
                             (define-key m (kbd "T") (elpaca-defsearch 'tried "#unique #installed !#declared"))
                             (define-key m (kbd "U") 'elpaca-ui-unmark)
                             (define-key m (kbd "b") 'elpaca-ui-browse-package)
                             (define-key m (kbd "d") 'elpaca-ui-mark-delete)
                             (define-key m (kbd "i") 'elpaca-ui-mark-install)
                             (define-key m (kbd "l") 'elpaca-log)
                             (define-key m (kbd "m") 'elpaca-manager)
                             (define-key m (kbd "r") 'elpaca-ui-mark-rebuild)
                             (define-key m (kbd "s") 'elpaca-ui-search)
                             (define-key m (kbd "t") 'elpaca-status)
                             (define-key m (kbd "u") 'elpaca-ui-mark-update)
                             (define-key m (kbd "v") 'elpaca-visit)
                             (define-key m (kbd "x") 'elpaca-ui-execute-marks)
                             m)
  "Keymap for `elpaca-ui-mode'.")
(defvar-local elpaca-ui--want-faces t "When non-nil, faces are applied to packages.")
(defvar-local elpaca-ui-search-filter nil "Filter for package searches.")
(defvar-local elpaca-ui-header-line-prefix nil "Header line prefix.")
(defvar-local elpaca-ui-header-line-function #'elpaca-ui--header-line
  "Function responsible for setting the UI buffer's `header-line-format'.
It recieves one argument, the parsed search query list.")
(defvar-local elpaca-ui-entries-function nil
  "Function responsible for returning the UI buffer's `tabulated-list-entries'.")
(defvar-local elpaca-ui--history nil "History for `elpaca-ui' minibuffer.")
(defvar url-http-end-of-headers)

(defvar elpaca-ui--string-cache nil
  "Cache for propertized strings.")

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
  (setq tabulated-list-printer #'elpaca-ui--apply-faces)
  (add-hook 'minibuffer-setup-hook 'elpaca-ui--minibuffer-setup)
  (hl-line-mode))

(defun elpaca-ui--minibuffer-setup ()
  "Set up the minibuffer for live filtering."
  (when-let ((buffer (with-minibuffer-selected-window
                       (and (derived-mode-p 'elpaca-ui-mode)
                            (eq this-command 'elpaca-ui-search)
                            (current-buffer)))))
    (add-hook 'post-command-hook (lambda () (elpaca-ui--debounce-search buffer)) nil :local)))

;;TODO: clean this up.
(defun elpaca-ui--parse-tokens (search)
  "Return a list of form ((TYPE (QUERY NEGATED))...) for SEARCH."
  (let* ((chars (mapcar #'string-to-char (split-string (string-trim search) "" 'omit-nulls)))
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
       ((eq char ?!)  (setq skip t
                            negatedp
                            (not (or lastp (member (nth (1+ i) chars) '(?\ ?|)))))))
      (unless skip (push char query))
      (when (or (not escapedp) lastp)
        (when-let ((query)
                   ((or (member char '(?\ ?|)) lastp))
                   (data (list (apply #'string (nreverse query)) negatedp)))
          (push (if tagp (push 'tag data) data) (if tagp queries col))
          (unless col (setq colcount -1))
          (setq query nil tagp nil negatedp nil))
        (when (and col (or (member char '(?| ?#)) lastp))
          (when (and lastp (>= colcount 0) (not (eq char ?|))) (cl-incf colcount))
          (push (if (eq colcount -1)
                    `(full-text ,@(nreverse col))
                  `(col ,colcount ,@(nreverse col)))
                queries)
          (when (eq char ?#) (setq colcount -1))
          (setq char nil col nil))))
    (nreverse queries)))

(defvar elpaca-ui--search-cache (make-hash-table :test #'equal))

(defun elpaca-ui--parse-search (search)
  "Parse SEARCH." ;by abusing the elisp reader
  (or (gethash search elpaca-ui--search-cache)
      (let ((adjustp (version< emacs-version "29"))
            ops chunk finished tagp negatedp)
        (with-temp-buffer
          (insert search)
          (goto-char (point-min))
          (while (not finished)
            (condition-case err
                (while t
                  (let ((op (read (current-buffer))))
                    (cond
                     ((or (symbolp op) (numberp op))
                      (when (numberp op) (setq op (intern (number-to-string op))))
                      (if (eq op '!)
                          (setq negatedp t)
                        (push (concat (and negatedp "!") (and tagp "#") (symbol-name op)) chunk)
                        (setq tagp nil negatedp nil)))
                     ((memq (car-safe op) '(quote \`))
                      (unless chunk (setq chunk (cons nil nil)))
                      (setcar chunk (concat (car chunk)
                                            (if (eq (car op) 'quote) "'" "`")
                                            (symbol-name (cadr op)))))
                     (t
                      (when chunk
                        (push (elpaca-ui--parse-tokens (string-join (nreverse chunk) " ")) ops))
                      (setq chunk nil)
                      (push `((elisp ,op)) ops)))))
              (end-of-file (setq finished t))
              (invalid-read-syntax (when adjustp (forward-char))
                                   (when (and (or (equal (cadr err) "#")
                                                  (string-prefix-p "integer" (cadr err)))
                                              (not (looking-back "#" nil)))
                                     (setq tagp t)
                                     (re-search-backward "#" nil 'noerror)
                                     (forward-char)))))
          (when chunk
            (push (elpaca-ui--parse-tokens (string-join (nreverse chunk) " ")) ops))
          (let ((parsed (apply #'append (nreverse ops))))
            (puthash search parsed elpaca-ui--search-cache)
            parsed)))))

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
                    body)))
           ((eq type 'elisp)
            (let ((sym (caar props))
                  (args (cdar props)))
              (push `(apply (function ,(or (alist-get sym elpaca-ui-search-tags) sym))
                            (list entries ,@args))
                    body))))
          (cl-incf i)))
      `(with-no-warnings
         (lambda ()
           (let ((entries (funcall elpaca-ui-entries-function)))
             ,@(mapcar (lambda (form) `(setq entries ,form)) (nreverse body))))))))

(defvar-local elpaca-ui--print-cache nil "Used when printing entries via `elpaca-ui--apply-faces'.")

(defun elpaca-ui--print ()
  "Print table entries."
  (let ((elpaca-ui--print-cache (append elpaca-ui--marked-packages (elpaca--queued))))
    (tabulated-list-print)))

(defun elpaca-ui--apply-faces (id cols)
  "Propertize entries which are marked/installed.
ID and COLS mandatory args to fulfill `tabulated-list-printer' API."
  (if-let ((name (propertize (aref cols 0) 'display nil))
           (found (cl-find (intern name) elpaca-ui--print-cache :key #'car))
           (target (cdr found))
           (result (if (elpaca<-p target) ;;not marked
                       (if elpaca-ui--want-faces
                           (propertize name 'face (elpaca-alist-get (elpaca--status target) elpaca-status-faces 'default))
                         name)
                     (let* ((props  (cdr target))
                            (face   (or (plist-get props :face) 'elpaca-ui-marked-package))
                            (prefix (or (plist-get props :prefix) "*")))
                       (propertize name 'display (propertize (concat prefix " " name) 'face face))))))
      (progn
        (setq cols (copy-tree cols t))
        (setf (aref cols 0) result))
    (remove-text-properties 0 (length (aref cols 0)) '(display) (aref cols 0)))
  (tabulated-list-print-entry id cols))

(defun elpaca-ui--apply-face ()
  "Apply face to current entry item."
  (when-let ((entry (get-text-property (point) 'tabulated-list-entry))
             (name  (aref entry 0))
             (item  (intern name))
             (offset (save-excursion
                       (goto-char (point-min))
                       (let ((continue t)
                             (line 0))
                         (while (and continue (not (eobp)))
                           (if (get-text-property (point) 'tabulated-list-entry)
                               (setq continue nil)
                             (cl-incf line))
                           (forward-line))
                         line)))
             (lines (cl-loop for i below (length tabulated-list-entries)
                             for entry = (nth i tabulated-list-entries)
                             when (eq (car entry) item) collect i)))
    (save-excursion
      (with-silent-modifications
        (cl-loop with marked = (cl-find item elpaca-ui--marked-packages :key #'car)
                 with props  = (nthcdr 2 marked)
                 with face   = (or (plist-get props :face) 'elpaca-ui-marked-package)
                 with prefix = (or (plist-get props :prefix) "*")
                 with mark   = (propertize (concat prefix " " name) 'face face)
                 with len    = (length name)
                 for line in lines
                 do (progn
                      (goto-char (point-min))
                      (forward-line (+ line offset))
                      (let* ((start (line-beginning-position))
                             (end (+ start len)))
                        (if marked
                            (put-text-property start (+ start (length name)) 'display mark)
                          (remove-text-properties start end '(display))))))))))

(defun elpaca-ui--update-search-filter (&optional buffer query)
  "Update the BUFFER to reflect search QUERY.
If QUERY is nil, the contents of the minibuffer are used instead."
  (let ((query (or query (and (minibufferp) (minibuffer-contents-no-properties))
                   elpaca-ui-search-filter elpaca-ui-default-query)))
    (with-current-buffer
        (get-buffer-create (or buffer (with-minibuffer-selected-window (current-buffer))))
      (when (string-empty-p query) (setq query elpaca-ui-default-query))
      (when-let ((parsed (elpaca-ui--parse-search query))
                 (fn (elpaca-ui--search-fn parsed)))
        (setq tabulated-list-entries (funcall (byte-compile fn))
              elpaca-ui-search-filter query))
      (elpaca-ui--print)
      (when elpaca-ui-header-line-function
        (setq header-line-format (funcall elpaca-ui-header-line-function
                                          elpaca-ui-header-line-prefix))))))

(defun elpaca-ui--debounce-search (buffer)
  "Update BUFFER's search filter from minibuffer."
  (let ((input (string-trim (minibuffer-contents-no-properties))))
    (unless (or (string-empty-p input)
                (string= input (with-current-buffer buffer elpaca-ui-search-filter)))
      (if elpaca-ui--search-timer
          (cancel-timer elpaca-ui--search-timer))
      (setq elpaca-ui--search-timer (run-at-time elpaca-ui-search-debounce-interval
                                                 nil
                                                 #'elpaca-ui--update-search-filter
                                                 buffer)))))

(defun elpaca-ui--ensure-mode ()
  "Ensure current buffer is derived from `elpaca-ui-mode'."
  (unless (derived-mode-p 'elpaca-ui-mode)
    (user-error "Cannot search outside of `elpaca-ui-mode'")))

(defvar elpaca-ui-search-prompt "Search (empty to clear): ")
(defun elpaca-ui-search (&optional query)
  "Filter current buffer by QUERY. If QUERY is nil, prompt for it."
  (interactive
   (progn (elpaca-ui--ensure-mode)
          (list (string-trim
                 (condition-case nil
                     (read-from-minibuffer elpaca-ui-search-prompt
                                           (and current-prefix-arg elpaca-ui-search-filter)
                                           nil nil elpaca-ui--history)
                   (quit elpaca-ui-search-filter))))))
  (elpaca-ui--ensure-mode)
  (when (string-empty-p query) (setq query elpaca-ui-default-query))
  (when (not (string= query elpaca-ui-search-filter))
    (setq elpaca-ui-search-filter query)
    (elpaca-ui--update-search-filter (current-buffer))))

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
  (or (get-text-property (point) 'tabulated-list-id) (user-error "No package at point")))

(defun elpaca-ui-browse-package ()
  "Browse current package's URL via `browse-url'."
  (interactive)
  (elpaca-browse (elpaca-ui-current-package)))

(defun elpaca-ui-package-marked-p (package)
  "Return t if PACKAGE is marked."
  (and (member package (mapcar #'car elpaca-ui--marked-packages)) t))

(defun elpaca-ui--unmark (package)
  "Unmark PACKAGE."
  (setq elpaca-ui--marked-packages
        (cl-remove-if (lambda (cell) (string= (car cell) package)) elpaca-ui--marked-packages))
  (elpaca-ui--apply-face))

(defun elpaca-ui-unmark ()
  "Unmark current package.
If region is active unmark all packages in region."
  (interactive)
  (elpaca-ui--unmark (elpaca-ui-current-package))
  (forward-line))

(defun elpaca-ui--mark (package action)
  "Internally mark PACKAGE for ACTION."
  (cl-pushnew (cons package (assoc action elpaca-ui-actions))
              elpaca-ui--marked-packages :key #'car)
  (elpaca-ui--apply-face))

(defun elpaca-ui-mark (package action)
  "Mark PACKAGE for ACTION with PREFIX.
ACTION is the description of a cell in `elpaca-ui-actions'.
The action's function is passed the name of the package as its sole argument."
  (interactive)
  (elpaca-ui--mark package action)
  (forward-line))

(defun elpaca-ui--toggle-mark (&optional test action)
  "Toggle ACTION mark for current package.
TEST is a unary function evaluated prior to toggling the mark.
The current package is its sole argument."
  (if-let ((package (elpaca-ui-current-package)))
      (progn
        (when test (funcall test package))
        (if (elpaca-ui-package-marked-p package)
            (elpaca-ui-unmark)
          (elpaca-ui-mark package action)))
    (user-error "No package associated with current line")))

(defmacro elpaca-ui-defmark (name test)
  "Define a marking command with NAME and TEST."
  (declare (indent 1) (debug t))
  `(defun ,(intern (format "elpaca-ui-mark-%s" name)) ()
     ,(format "Mark package for %s action." name)
     (interactive)
     (if (not (use-region-p))
         (elpaca-ui--toggle-mark ,test ',name)
       (let ((end (region-end))
             (beg (region-beginning)))
         (save-restriction
           (goto-char beg)
           (while (not (>= (point) end))
             (condition-case _
                 (elpaca-ui--toggle-mark ,test ',name)
               ((error) (forward-line))))
           (deactivate-mark))))))

(elpaca-ui-defmark rebuild
  (lambda (p) (unless (or (elpaca-installed-p p) (alist-get p (elpaca--queued)))
                (user-error "Package %S is not installed" p))))

(elpaca-ui-defmark install
  (lambda (p) (when (elpaca-installed-p p) (user-error "Package %S already installed" p))))

(elpaca-ui-defmark update
  (lambda (p) (unless (elpaca-installed-p p) (user-error "Package %S is not installed" p))))

(elpaca-ui-defmark delete
  (lambda (p) (unless (or (elpaca-installed-p p)
                          (alist-get p (elpaca--queued))
                          (get-text-property (point) 'orphan-dir))
                (user-error "Package %S is not installed" p))))

(declare-function elpaca-log--latest "elpaca-log")
(defvar elpaca-manager-buffer)
(defun elpaca-ui--post-execute ()
  "Refresh views."
  (require 'elpaca-log)
  (require 'elpaca-manager)
  (when-let ((buffer (get-buffer elpaca-manager-buffer)))
    (with-current-buffer buffer
      (when (functionp elpaca-ui-entries-function)
        (funcall elpaca-ui-entries-function))
      (elpaca-ui-search-refresh buffer))))

(defun elpaca-ui-execute-marks ()
  "Execute each action in `elpaca-ui-marked-packages'."
  (interactive)
  (when (null elpaca-ui--marked-packages) (user-error "No marked packages"))
  (deactivate-mark)
  (cl-loop with setups
           with actions
           for (item _ . props) in elpaca-ui--marked-packages
           for action = (plist-get props :action)
           for setup = (plist-get props :setup)
           when (functionp setup) do (cl-pushnew setup setups)
           when action do (cl-pushnew (list action item) actions)
           (pop elpaca-ui--marked-packages)
           finally do
           (mapc #'funcall (nreverse setups))
           (mapc #'apply actions))
  (when-let ((q (cl-find-if (lambda (q) (and (eq (elpaca-q<-status q) 'incomplete)
                                             (elpaca-q<-elpacas q)))
                            elpaca--queues)))
    (setf (elpaca-q<-post q) #'elpaca-ui--post-execute))
  (elpaca-process-queues))

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

(provide 'elpaca-ui)
;;; elpaca-ui.el ends here
