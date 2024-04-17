;;; elpaca-ui.el --- package UI for elpaca.el  -*- lexical-binding: t; -*-

;;; Commentary:
;; Package search, maintenance UI.
(require 'elpaca)
(require 'url)
(require 'tabulated-list)

;;; Code:
(defgroup elpaca-ui nil "Elpaca's UI options." :group 'elpaca)

(defface elpaca-ui-marked-delete '((t (:inherit error :inverse-video t)))
  "Face for packages marked for deletion.")
(defface elpaca-ui-marked-install '((t (:inherit highlight :weight bold)))
  "Face for packages marked for installation.")
(defface elpaca-ui-marked-rebuild '((t (:inherit match :weight bold)))
  "Face for packages marked for rebuild.")
(defface elpaca-ui-marked-fetch '((t (:inherit secondary-selection :weight bold)))
  "Face for packages marked for fetch.")
(defface elpaca-ui-marked-merge '((t (:inherit region :weight bold :inverse-video t)))
  "Face for packages marked for merging.")
(defface elpaca-ui-marked-pull '((t (:inherit warning :inverse-video t)))
  "Face for packages marked for pulling.")

(defcustom elpaca-ui-row-limit 1000 "Max rows to print at once." :type 'integer)
(defcustom elpaca-ui-default-query ".*" "Initial `elpaca-ui-mode' search query."
  :type 'string)
(make-variable-buffer-local 'elpaca-ui-default-query)

(defcustom elpaca-ui-marks
  '((elpaca-delete  :prefix "💀" :face elpaca-ui-marked-delete :args (id 'force 'deps))
    (elpaca-try     :prefix "⚙️" :face elpaca-ui-marked-install)
    (elpaca-rebuild :prefix "♻️️" :face elpaca-ui-marked-rebuild)
    (elpaca-fetch   :prefix "‍🐕‍🦺" :face elpaca-ui-marked-fetch)
    (elpaca-merge   :prefix "🤝" :face elpaca-ui-marked-merge :args (id prefix-arg))
    (elpaca-pull    :prefix "⬆️" :face elpaca-ui-marked-pull :args (id prefix-arg)))

  "List of marks which can be applied to packages `elpaca-ui-mode' buffers.
Each element is of the form (COMMAND :KEY VAL...).
Accepted key val pairs are:
  - :prefix STRING inserted to indicate mark in UI
  - :face FACE for marked row in UI
  - :args (ARG...) arguments passed to COMMAND.
      `id` is replaced with the package ID.
      `prefix-arg` is replaced with `current-prefix-arg' at time of marking."
  :type '(list (function :tag "command") plist))

(defvar-local elpaca-ui--marked-packages nil "Aist of buffer's marked packages.")

(defun elpaca-ui--tag-dirty (entries)
  "Return ENTRIES for packages with a dirty worktree."
  (cl-remove-if-not #'elpaca-worktree-dirty-p entries :key #'car))

(defun elpaca-ui--tag-declared (entries)
  "Return ENTRIES for packages declared during init."
  (cl-remove-if-not #'elpaca-declared-p entries :key #'car))

(defun elpaca-ui--tag-orphan (_)
  "Return entires for packages not temporarlily installed or declared."
  (let ((repos (nthcdr 2 ; Discard "." ".."
                       (mapcar #'file-name-as-directory
                               (directory-files elpaca-repos-directory t)))))
    (mapcar (lambda (dir)
              (let ((name (file-name-nondirectory (directory-file-name dir))))
                (list (intern name) (vector (propertize name 'orphan-dir dir)
                                            "orphan package" "n/a" "n/a" "n/a"))))
            (cl-set-difference (cl-remove-if-not #'file-directory-p repos)
                               (mapcar (lambda (q) (elpaca<-repo-dir (cdr q)))
                                       (elpaca--queued))
                               :test #'equal))))

(defun elpaca-ui--tag-random (entries &optional limit)
  "Return LIMIT random ENTRIES."
  (if (< (length entries) (or limit 10))
      entries
    (cl-loop with (results seen)
             until (= (length results) (or limit 10))
             for n = (random (length entries))
             unless (memq n seen) do (push (nth n entries) results)
             (push n seen)
             finally return results)))

(defun elpaca-ui--tag-installed (entries)
  "Return ENTRIES for installed packages."
  (cl-remove-if-not #'elpaca-installed-p entries :key #'car))

(defun elpaca-ui--tag-marked (entries)
  "Return ENTRIES for marked packages."
  (cl-loop for (id . _) in elpaca-ui--marked-packages collect (assoc id entries)))

(defun elpaca-ui--tag-unique (entries)
  "Return last occurrence of each entry in ENTRIES."
  (cl-remove-duplicates entries :key #'car :from-end t))

(defcustom elpaca-ui-search-tags '((dirty     . elpaca-ui--tag-dirty)
                                   (declared  . elpaca-ui--tag-declared)
                                   (orphan    . elpaca-ui--tag-orphan)
                                   (unique    . elpaca-ui--tag-unique)
                                   (random    . elpaca-ui--tag-random)
                                   (installed . elpaca-ui--tag-installed)
                                   (marked    . elpaca-ui--tag-marked))
  "Alist of search tags.
Each cell is of form (NAME FILTER).
FILTER function must take `tabulated-list-entries' as its first argument.
It must return list of `tabulated-list-entries' or nil.

Each tag can be inverted in the minibuffer by prepending an
exclamation point to it. e.g. !#installed."
  :type '(alist :key-type symbol :value-type function))

(defcustom elpaca-ui-search-debounce-interval 0.25
  "Length of time in seconds to wait before updating the search UI."
  :type (or 'string 'int 'float))

(defmacro elpaca-defsearch (name query)
  "Return search command with NAME for QUERY."
  (declare (indent 1) (debug t))
  `(defun ,(intern (format "elpaca-ui-search-%s" name)) ()
     ,(format "Search for %S" query)
     (interactive)
     (elpaca-ui-search ,query)))

(defun elpaca-ui--button-noop (&rest args)
  "Return first arg in ARGS."
  (car args))

(defalias 'elpaca-ui--buttonize
  (with-no-warnings (cond
                     ;;API for Emacs 27 requires allocating temp buffers. Not worth it.
                     ((< emacs-major-version 28) #'elpaca-ui--button-noop)
                     ((< emacs-major-version 29) #'button-buttonize)
                     (t #'buttonize))))

(defcustom elpaca-ui-waiting-indicator
  (propertize
   (elpaca-ui--buttonize "⚠️" (lambda (&rest _) (call-interactively #'keyboard-quit)) nil)
   'help-echo "Blocking due to elpaca-wait. \\[keyboard-quit] to quit.")
  "Indicator shown in progress bar when `elpaca-wait' is polling."
  :type (or 'string 'nil))

;;;; Variables:
(defvar-local elpaca-ui--search-timer nil "Timer to debounce search input.")
(defvar-local elpaca-ui--prev-entry-count nil "Number of previously recorded entries.")

(defvar elpaca-ui-view-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "a") (elpaca-defsearch marked "#unique #marked"))
    (define-key m (kbd "i") (elpaca-defsearch installed "#unique #installed"))
    (define-key m (kbd "l") 'elpaca-log)
    (define-key m (kbd "m") 'elpaca-manager)
    (define-key m (kbd "o") (elpaca-defsearch orphaned "#unique #orphan"))
    (define-key m (kbd "r") 'elpaca-ui-search-refresh)
    (define-key m (kbd "t") (elpaca-defsearch tried "#unique #installed !#declared"))
    m)
  "Keymap for `elpaca-ui-mode' views.")

(defvar elpaca-ui-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "RET") 'elpaca-ui-info)
    (define-key m (kbd "!") 'elpaca-ui-send-input)
    (define-key m (kbd "+") 'elpaca-ui-show-hidden-rows)
    (define-key m (kbd "b") 'elpaca-ui-browse-package)
    (define-key m (kbd "d") 'elpaca-ui-mark-delete)
    (define-key m (kbd "f") 'elpaca-ui-mark-fetch)
    (define-key m (kbd "g") elpaca-ui-view-map)
    (define-key m (kbd "i") 'elpaca-ui-mark-try)
    (define-key m (kbd "m") 'elpaca-ui-mark-merge)
    (define-key m (kbd "p") 'elpaca-ui-mark-pull)
    (define-key m (kbd "r") 'elpaca-ui-mark-rebuild)
    (define-key m (kbd "s") 'elpaca-ui-search)
    (define-key m (kbd "u") 'elpaca-ui-unmark)
    (define-key m (kbd "v") 'elpaca-ui-visit)
    (define-key m (kbd "x") 'elpaca-ui-execute-marks)
    m)
  "Keymap for `elpaca-ui-mode'.")

(defvar-local elpaca-ui--want-faces t "When non-nil, faces are applied to packages.")
(defvar-local elpaca-ui-search-query nil "Package search query.")
(defvar-local elpaca-ui-header-line-prefix nil "Header line prefix.")
(defvar-local elpaca-ui-header-line-function #'elpaca-ui--header-line
  "Function responsible for setting the UI buffer's `header-line-format'.
It receives one argument, the parsed search query list.")
(defvar-local elpaca-ui-entries-function nil
  "Function responsible for returning the UI buffer's `tabulated-list-entries'.")
(defvar-local elpaca-ui-entries nil "List of table entries.")
(defvar-local elpaca-ui--history nil "History for `elpaca-ui' minibuffer.")
(defvar elpaca-ui--string-cache nil "Cache for propertized strings.")
(defvar url-http-end-of-headers)
(defvar elpaca-ui--progress-bar-e (propertize "E:" 'face '(:weight bold)))
(defvar elpaca-ui--pbh-cache nil "Progress bar help echo cache.")
(defvar elpaca-ui--pbh-timer nil "Progress bar help echo timer.")

;;;; Functions:
(defun elpaca-ui--pbh (_ string pos)
  "Return packages with status at STRING POS."
  (let ((status (get-text-property pos 'status string)))
    (or
     (alist-get status elpaca-ui--pbh-cache)
     (setf elpaca-ui--pbh-timer
           (progn (when elpaca-ui--pbh-timer (cancel-timer elpaca-ui--pbh-timer))
                  (run-at-time 0.5 nil (lambda () (setq elpaca-ui--pbh-cache nil))))
           (alist-get status elpaca-ui--pbh-cache)
           (concat
            (symbol-name status) " orders\n"
            (cl-loop
             with es = (mapcar #'cdr (elpaca--queued))
             with orders =
             (cl-sort (if (eq status 'other)
                          (cl-remove-if (lambda (e) (memq (elpaca--status e)
                                                          elpaca--inactive-states))
                                        es)
                        (cl-remove-if-not (lambda (e) (eq (elpaca--status e) status)) es))
                      #'string< :key #'cadr)
             with len = (length orders)
             with limit = (1- len)
             for i below len concat (concat (elpaca<-package (nth i orders))
                                            (unless (eq i limit) ",")
                                            (if (and  (= 0 (mod (1+ i) 5))) "\n" " "))))))))

(defun elpaca-ui--progress-bar ()
  "Return string indicating state of queues."
  (cl-loop
   with counts = nil with total = 0 with finalized = 0
   for s in '(finished blocked failed other)
   for plen = (elpaca-alist-get s elpaca--status-counts 0)
   for count = (propertize (number-to-string plen)
                           'face (elpaca-alist-get s elpaca-status-faces '(:weight bold))
                           'status s 'help-echo-inhibit-substitution t 'help-echo #'elpaca-ui--pbh)
   do (setq counts (concat counts " " count) total (+ total plen))
   (when (memq s '(finished failed)) (cl-incf finalized plen))
   finally return
   (concat (when elpaca--waiting elpaca-ui-waiting-indicator)
           counts "|" (format "%6.2f%%%%" (* 100 (/ (float finalized) (max total 1)))) "|")))

(defvar elpaca-ui--header-line-matching (propertize "matching:" 'face '(:weight bold)))
(defun elpaca-ui--header-line (&optional prefix)
  "Set `header-line-format' to reflect query.
If PREFIX is non-nil it is displayed before the rest of the header-line."
  (let* ((tlen (length tabulated-list-entries))
         (hlen (- (length elpaca-ui-entries) tlen))
         (hidden (when (> hlen 0)
                   (elpaca-ui--buttonize (concat "(+" (number-to-string hlen) ")")
                                         (lambda (_) (elpaca-ui-show-hidden-rows))))))
    (setq header-line-format
          (string-join (list (elpaca-ui--progress-bar) prefix (number-to-string tlen)
                             hidden elpaca-ui--header-line-matching elpaca-ui-search-query)
                       " "))))

(define-derived-mode elpaca-ui-mode tabulated-list-mode "elpaca-ui"
  "Major mode to manage packages."
  (setq tabulated-list-printer #'elpaca-ui--apply-faces)
  (add-hook 'minibuffer-setup-hook 'elpaca-ui--minibuffer-setup)
  (elpaca-ui-live-update-mode 1)
  (advice-add #'tabulated-list-print :after #'elpaca-ui--print-appender)
  (hl-line-mode))

(define-minor-mode elpaca-ui-live-update-mode "Filters results as query is typed."
  :lighter " elui")

(defun elpaca-ui--minibuffer-setup ()
  "Set up the minibuffer for live filtering."
  (when-let ((buffer (with-minibuffer-selected-window
                       (and elpaca-ui-live-update-mode
                            (derived-mode-p 'elpaca-ui-mode)
                            (eq this-command 'elpaca-ui-search)
                            (current-buffer)))))
    (add-hook 'post-command-hook (lambda () (ignore-errors (elpaca-ui--debounce-search buffer))) nil :local)))

(defvar elpaca-ui--search-cache (make-hash-table :test #'equal))
(defun elpaca-ui--lex-query (query &optional nocache)
  "Return list of tokens from QUERY. If NOCACHE is non-nil, bypass cache."
  (or (unless nocache (gethash query elpaca-ui--search-cache))
      (with-current-buffer (get-buffer-create " *elpaca-ui--lex-query*")
        (erase-buffer)
        (insert (string-trim query))
        (goto-char (point-min))
        (let ((colcount -1) prev tokens token negated-p first)
          (while (not (eobp))
            (setq prev (point) negated-p nil)
            (skip-chars-forward "^ ")
            (when (eq (char-before (point)) ?\\) (skip-chars-forward "^ "))
            (when (looking-back "\\(?:[[:space:]]*#([^z-a]*\\)" prev)
              (skip-chars-backward "^#") (forward-sexp))
            (setq token (string-trim (buffer-substring-no-properties prev (point))))
            (when (= (aref token 0) ?!) (setq token (substring token 1) negated-p t))
            (setq first (aref token 0))
            (push (if (not (equal first ?|))
                      (list (if (= first ?#) (ignore-errors (read (substring token 1)))
                              token)
                            negated-p)
                    (when (> (length token) 1) (backward-char (length token)))
                    (cl-incf colcount (if (= colcount -1) 2 1)))
                  tokens)
            (ignore-errors (forward-char 1)))
          (cl-loop for token in tokens if (numberp token) do (cl-decf colcount)
                   else collect (push colcount token))))))

(defun elpaca-ui--col-search (index queries)
  "Return columnar search for column at INDEX with QUERIES."
  `(cl-loop for entry in entries for data = ,@(if (= index -1)
                                                  '((string-join (cadr entry) " "))
                                                '((cadr entry)))
            when (and ,@queries) collect entry))

(defun elpaca-ui--search-fn (tokens)
  "Return query function from TOKENS."
  (cl-loop
   with (fns column)
   with i = (caar tokens)
   for (col token negated) in tokens
   when token do ;; guard against empty sexp, #()
   (cond ((stringp token) ;; string query
          (unless (or (= col i) (> col -1))
            (setq fns (cons (elpaca-ui--col-search i column) fns) i col column nil))
          (let ((query `(string-match-p ,token ,(if (= -1 col) 'data `(aref data ,col)))))
            (push (if negated `(not ,query) query) column)))
         ((or (consp token) (symbolp token)) ;; elisp or tag
          (when column (setq fns (cons (elpaca-ui--col-search i column) fns) column nil))
          (let* ((sym (or (car-safe token) token))
                 (fn (if (eq sym 'lambda)
                         `(funcall ,token entries)
                       `(apply (function ,(or (alist-get sym elpaca-ui-search-tags)
                                              (user-error "%s tag not found" sym)))
                               (list entries ,@(cdr-safe token))))))
            (push (if negated `(cl-set-difference entries ,fn) fn) fns))))
   finally do (when column (push (elpaca-ui--col-search i column) fns))
   finally return (when fns
                    `(with-no-warnings
                       (lambda ()
                         (let ((entries (funcall elpaca-ui-entries-function)))
                           (setq ,@(cl-loop for fn in fns append `(entries ,fn)))))))))

(defvar-local elpaca-ui--print-cache nil "Used when printing entries via `elpaca-ui--apply-faces'.")
(define-minor-mode elpaca-ui-tail-mode "Automatically follow tail of UI buffer when enabled."
  :lighter " elpaca-ui-tail")

(defun elpaca-ui--print-appender (&rest _)
  "Prints button to append more `elpaca-ui-entries' rows."
  (when-let (((derived-mode-p 'elpaca-ui-mode))
             (tlen (length tabulated-list-entries))
             (elen (length elpaca-ui-entries))
             ((< tlen elen))
             (s (propertize (format "+ %d more rows..." (- elen tlen))
                            'face '(:weight bold))))
    (save-excursion
      (goto-char (point-max))
      (with-silent-modifications
        (insert (elpaca-ui--buttonize s (lambda (&rest _) (elpaca-ui-show-hidden-rows))))))))

(defun elpaca-ui-show-hidden-rows (&optional n)
  "Append rows up to N times `elpaca-ui-row-limit'."
  (interactive "p")
  (if-let ((tlen (length tabulated-list-entries))
           (elen (length elpaca-ui-entries))
           ((< tlen elen)))
      (let ((elpaca-ui--print-cache (append elpaca-ui--marked-packages (elpaca--queued)))
            (inhibit-read-only t)
            (limit (or elpaca-ui-row-limit most-positive-fixnum)))
        (goto-char (point-max))
        (delete-region (line-beginning-position) (line-end-position))
        (when-let ((sorter (tabulated-list--get-sorter)))
          (setq tabulated-list-entries (sort tabulated-list-entries sorter)))
        (dotimes (i (min (* limit (or n 1)) (- elen tlen)))
          (when-let ((entry (nth (+ i tlen) elpaca-ui-entries)))
            (setcdr (last tabulated-list-entries) (cons entry nil))
            (elpaca-ui--apply-faces (car entry) (cadr entry))))
        (elpaca-ui--print-appender)
        (elpaca-ui--header-line elpaca-ui-header-line-prefix))
    (user-error "End of table")))

(defun elpaca-ui--print ()
  "Print table entries."
  (let ((elpaca-ui--print-cache (append elpaca-ui--marked-packages (elpaca--queued)))
        (p (point)))
    (tabulated-list-print)
    (goto-char (if elpaca-ui-tail-mode (point-max) p))))

(defun elpaca-ui--apply-faces (id cols)
  "Propertize entries which are marked/installed.
ID and COLS mandatory args to fulfill `tabulated-list-printer' API."
  (if-let ((name (propertize (aref cols 0) 'display nil))
           (namesym (intern name))
           (found (cl-loop for it in elpaca-ui--print-cache thereis (and (eq namesym (car it)) it)))
           (target (cdr found))
           (result (if (elpaca-p target) ;;not marked
                       (if elpaca-ui--want-faces
                           (propertize name 'face (elpaca-alist-get (elpaca--status target) elpaca-status-faces 'default))
                         name)
                     (let* ((props  (cdr target))
                            (face   (or (plist-get props :face) 'default))
                            (prefix (or (plist-get props :prefix) "*")))
                       (propertize name 'display (propertize (concat prefix " " name) 'face face))))))
      (progn
        (setq cols (copy-tree cols t))
        (setf (aref cols 0) result))
    (remove-text-properties 0 (length (aref cols 0)) '(display) (aref cols 0)))
  (tabulated-list-print-entry id cols))

(defun elpaca-ui--apply-face ()
  "Apply face to current entry id."
  (when-let ((entry (tabulated-list-get-entry))
             (name  (aref entry 0))
             (id    (intern name))
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
                             when (eq (car entry) id) collect i)))
    (save-excursion
      (with-silent-modifications
        (cl-loop
         with marked = (cl-find id elpaca-ui--marked-packages :key #'car)
         with props  = (nthcdr 2 marked)
         with face   = (or (plist-get props :face) 'default)
         with prefix = (or (plist-get props :prefix) "*")
         with parg   = (plist-get props :prefix-arg)
         with mark   = (propertize (concat prefix (when parg "+") " " name) 'face face)
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

(defun elpaca-ui--update-search-query (&optional buffer query)
  "Update the BUFFER to reflect search QUERY.
If QUERY is nil, the contents of the minibuffer are used instead."
  (let ((query (or query (and (minibufferp) (minibuffer-contents-no-properties))
                   elpaca-ui-search-query elpaca-ui-default-query))
        (b (or buffer (with-minibuffer-selected-window (current-buffer)) (current-buffer))))
    (with-current-buffer (get-buffer-create b)
      (when (string-empty-p query) (setq query elpaca-ui-default-query))
      (when-let ((parsed (ignore-errors (elpaca-ui--lex-query query)))
                 (fn (elpaca-ui--search-fn parsed)))
        (let ((entries (funcall (byte-compile fn))))
          (when-let ((fn (tabulated-list--get-sorter))) (setq entries (sort entries fn)))
          (setq elpaca-ui-entries entries
                tabulated-list-entries
                (if-let ((elen (length elpaca-ui-entries))
                         ((or (not elpaca-ui-row-limit) (<= elen elpaca-ui-row-limit))))
                    elpaca-ui-entries
                  (cl-subseq elpaca-ui-entries 0 (min elpaca-ui-row-limit elen)))
                elpaca-ui-search-query query))
        (elpaca-ui--print)
        (when elpaca-ui-header-line-function
          (setq header-line-format (funcall elpaca-ui-header-line-function
                                            elpaca-ui-header-line-prefix)))))))

(defun elpaca-ui--debounce-search (buffer)
  "Update BUFFER's search query from minibuffer."
  (let ((input (string-trim (minibuffer-contents-no-properties))))
    (unless (or (string-empty-p input)
                (string= input (with-current-buffer buffer elpaca-ui-search-query)))
      (when elpaca-ui--search-timer (cancel-timer elpaca-ui--search-timer))
      (setq elpaca-ui--search-timer
            (run-at-time elpaca-ui-search-debounce-interval nil
                         (lambda (buffer) (with-demoted-errors "elpaca-ui-search:...%S"
                                            (elpaca-ui--update-search-query buffer)))
                         buffer)))))

(defun elpaca-ui--ensure-mode ()
  "Ensure current buffer is derived from `elpaca-ui-mode'."
  (or (derived-mode-p 'elpaca-ui-mode) (user-error "Buffer not in `elpaca-ui-mode'")))

(defun elpaca-ui--tag-annotator (tag)
  "Annotate TAG."
  (when-let ((fn (alist-get tag elpaca-ui-search-tags nil nil #'string=))
             (doc (documentation fn)))
    (concat " " (substring doc 0 (string-search "\n" doc)))))

(defvar elpaca-ui-search-prompt "Search (empty to clear): ")

(defun elpaca-ui--complete-tag ()
  "Return `elpaca-ui-search-tags' as completion candidates."
  (and (looking-back "\\(?:#[[:alpha:]]*\\)" 0)
       (list (save-excursion (re-search-backward "#") (1+ (point)))
             (point)
             (with-minibuffer-selected-window elpaca-ui-search-tags)
             :annotation-function #'elpaca-ui--tag-annotator)))

(defun elpaca-ui-search (&optional query)
  "Filter current buffer by QUERY. If QUERY is nil, prompt for it."
  (interactive
   (let ((completion-at-point-functions
          (cons #'elpaca-ui--complete-tag completion-at-point-functions)))
     (elpaca-ui--ensure-mode)
     (list (string-trim
            (condition-case nil
                (read-from-minibuffer elpaca-ui-search-prompt
                                      (and current-prefix-arg elpaca-ui-search-query)
                                      nil nil elpaca-ui--history)
              (quit elpaca-ui-search-query))))))
  (elpaca-ui--ensure-mode)
  (when (string-empty-p query) (setq query elpaca-ui-default-query))
  (unless (string= query elpaca-ui-search-query)
    (setq elpaca-ui-search-query query)
    (elpaca-ui--update-search-query (current-buffer))))

(defun elpaca-ui-search-refresh (&optional buffer silent)
  "Rerun the current search for BUFFER.
If BUFFER is non-nil, the current buffer is used.
If SILENT is non-nil, suppress update message."
  (interactive (list (current-buffer)))
  (with-current-buffer (or buffer (current-buffer))
    (elpaca-ui--update-search-query (or buffer (current-buffer))
                                    (or elpaca-ui-search-query
                                        elpaca-ui-default-query))
    (unless silent (message "Search %S refreshed" elpaca-ui-search-query))))

(defun elpaca-ui-current-package ()
  "Return current package of UI line."
  (or (tabulated-list-get-id) (user-error "No package at point")))

(defun elpaca-ui-browse-package ()
  "Browse current package's URL via `browse-url'."
  (interactive)
  (elpaca-browse (elpaca-ui-current-package)))

(defun elpaca-ui-visit (&optional build)
  "Visit current package's repo or BUILD directory."
  (interactive (list current-prefix-arg))
  (elpaca-visit (elpaca-ui-current-package) build))

(defun elpaca-ui-package-marked-p (package)
  "Return t if PACKAGE is marked."
  (and (memq package (mapcar #'car elpaca-ui--marked-packages)) t))

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

(defun elpaca-ui--mark (package command)
  "Internally mark PACKAGE for COMMAND."
  (setf (alist-get package elpaca-ui--marked-packages)
        (append (assoc command elpaca-ui-marks) (list :prefix-arg current-prefix-arg)))
  (elpaca-ui--apply-face))

(defun elpaca-ui-mark (package command)
  "Mark PACKAGE for COMMAND."
  (interactive)
  (elpaca-ui--mark package command)
  (forward-line))

(defun elpaca-ui--toggle-mark (&optional test command)
  "Toggle COMMAND mark for current package.
TEST is a unary function evaluated prior to toggling the mark.
The current package is its sole argument."
  (let ((package (elpaca-ui-current-package)))
    (when test (funcall test package))
    (if (eq (car (alist-get package elpaca-ui--marked-packages)) command)
        (elpaca-ui-unmark)
      (elpaca-ui-mark package command))))

(defmacro elpaca-ui-defmark (name test)
  "Define a marking command with NAME and TEST."
  (declare (indent 1) (debug t))
  `(defun ,(intern (format "elpaca-ui-mark-%s"
                           (replace-regexp-in-string "^elpaca-" "" (symbol-name name))))
       () ,(format "Mark package at point for `%s'." name)
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

;;@TODO: Most of these commands should not be allowed while building is in process
(elpaca-ui-defmark elpaca-rebuild
  (lambda (p) (unless (or (elpaca-installed-p p) (alist-get p (elpaca--queued)))
                (user-error "Package %S is not installed" p))))

(defun elpaca-ui--ensure-installed (id)
  "Throw user error if package associted with ID is not installed."
  (unless (elpaca-installed-p id) (user-error "Package %S is not installed" id)))

(elpaca-ui-defmark elpaca-fetch #'elpaca-ui--ensure-installed)
(elpaca-ui-defmark elpaca-merge #'elpaca-ui--ensure-installed)
(elpaca-ui-defmark elpaca-pull  #'elpaca-ui--ensure-installed)

(elpaca-ui-defmark elpaca-try
  (lambda (p) (when (elpaca-installed-p p) (user-error "Package %S already installed" p))))

(elpaca-ui-defmark elpaca-delete
  (lambda (p) (unless (or (elpaca-installed-p p)
                          (alist-get p (elpaca--queued))
                          (get-text-property (point) 'orphan-dir))
                (user-error "Package %S is not installed" p))))

(defvar elpaca-manager-buffer)
(defvar elpaca-log-buffer)
(defun elpaca-ui--post-execute ()
  "Refresh views."
  (require 'elpaca-log)
  (require 'elpaca-manager)
  (when-let ((buffer (get-buffer elpaca-manager-buffer)))
    (with-current-buffer buffer
      (when (functionp elpaca-ui-entries-function)
        (funcall elpaca-ui-entries-function))
      (elpaca-ui-search-refresh buffer)))
  (when-let ((buffer (get-buffer elpaca-log-buffer)))
    (with-current-buffer buffer
      (when (functionp elpaca-ui-entries-function)
        (funcall elpaca-ui-entries-function))
      (elpaca-ui-search-refresh buffer))))

(defun elpaca-ui-execute-marks () ;;@TODO: make more flexible with regard to :args, :prefix-arg
  "Execute each mark in `elpaca-ui-marked-packages'."
  (interactive)
  (when (null elpaca-ui--marked-packages) (user-error "No marked packages"))
  (cl-loop initially do (deactivate-mark) (elpaca--maybe-log)
           for (id command . props) in elpaca-ui--marked-packages
           for args = (cl-loop for arg in (plist-get props :args) collect
                               (cond ((eq arg 'id) id)
                                     ((eq arg 'prefix-arg) (plist-get props :prefix-arg))
                                     (t arg)))
           do (apply command (or args (list id)))
           (pop elpaca-ui--marked-packages)
           finally (setq elpaca--post-queues-hook '(elpaca-ui--post-execute)))
  (let (elpaca-log-functions)
    (elpaca-process-queues (lambda (qs) (cl-remove-if-not #'elpaca-q<-elpacas qs)))))

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

(declare-function elpaca-info "elpaca-info")
(defun elpaca-ui-info ()
  "Show info for current package."
  (interactive)
  (elpaca-info (elpaca-ui-current-package)
               (get-text-property 0 'menu (aref (tabulated-list-get-entry) 3))))

(provide 'elpaca-ui)
;;; elpaca-ui.el ends here
