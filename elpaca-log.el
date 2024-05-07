;;; elpaca-log.el --- Elpaca Logging facilities.  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024 Nicholas Vollmer

;; Author:  Nicholas Vollmer
;; Keywords:

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

;;

;;; Code:
(require 'elpaca-ui)
(defvar elpaca-log-buffer "*elpaca-log*")
(defvar elpaca-log--history nil "`elpaca-log' minibuffer history.")
(defface elpaca-log-highlight '((t (:inherit warning))) "Highlight log info." :group 'elpaca-ui)
(defface elpaca-log-error '((t (:inherit error))) "Highlight log errors." :group 'elpaca-ui)
(defface elpaca-log-info '((t (:inherit shadow))) "Face for log info." :group 'elpaca-ui)

(defcustom elpaca-log-default-search-query "#unique" "Default query for `elpaca-log-buffer'."
  :type 'string :group 'elpaca-ui)

(defcustom elpaca-log-search-tags
  '((verbosity . elpaca-log--verbosity)
    (latest . elpaca-log--tag-latest)
    (linked-errors . elpaca-log--byte-comp-warnings)
    (update-log . elpaca-log--updates))
  "Alist of search tags (see `elpaca-ui-search-tags') exclusive to the log buffer."
  :type '(alist :key-type symbol :value-type function) :group 'elpaca-ui)

(defcustom elpaca-log-command-queries
  '(((elpaca-fetch elpaca-fetch-all elpaca-log-updates) . "#latest #update-log")
    ((elpaca-try elpaca-rebuild) . "#latest #linked-errors")
    (( elpaca-merge elpaca-merge-all elpaca-pull elpaca-pull-all
       elpaca-update elpaca-update-all)
     . "#latest #unique")
    ((eval-buffer eval-region eval-defun eval-last-sexp org-ctrl-c-ctrl-c) . silent)
    (elpaca-delete . (lambda () (if (equal (buffer-name) elpaca-log-buffer)
                                    elpaca-ui-search-query 'silent)))
    (elpaca-ui-execute-marks . elpaca-log--marked-query))
  "Alist of form ((COMMAND-OR-COMMAND-LIST . QUERY-OR-FUNCTION)...).
If query is a string it is used when logging for that command.
If it is a function, it's return value is used."
  :type 'alist :group 'elpaca-ui)

(defcustom elpaca-log-diff-function #'elpaca-log-diff
  "Function to display a diff from the update log.
It must accept a package ID symbol and REF string as its first two arguments."
  :type 'function :group 'elpaca-ui)

(defun elpaca-log--marked-query ()
  "Return query for marked packages."
  (when (= (length (delete-dups (mapcar #'cadr elpaca-ui--marked-packages))) 1)
    (let ((this-command (cadar elpaca-ui--marked-packages))) (elpaca-log-command-query))))

(defun elpaca-log--find-command (val key)
  "Return t if KEY VAL."
  (or (eq key val) (and (listp val) (member key val))))

;;;###autoload
(defun elpaca-log-command-query ()
  "Return logging query matching `this-command' in `elpaca-log-command-queries'."
  (when-let ((found (alist-get this-command elpaca-log-command-queries
                               nil nil #'elpaca-log--find-command)))
    (if (functionp found) (funcall found) found)))

;;;###autoload
(defun elpaca-log-initial-queues ()
  "Return logging query if initial queues require building or order fails."
  (unless elpaca-after-init-time
    (cl-loop for (_ . e) in (elpaca--queued)
             for query = (cond ((not (elpaca<-builtp e)) "#unique | !finished")
                               ((eq (elpaca--status e) 'failed) "| failed"))
             when query return
             (prog1 query
               (setq initial-buffer-choice
                     (let ((ibc initial-buffer-choice))
                       (lambda ()
                         (add-hook 'elpaca-after-init-hook
                                   (lambda () (setq initial-buffer-choice ibc)))
                         (get-buffer-create "*elpaca-log*"))))))))

(defun elpaca-log--tag-latest (entries)
  "Log ENTRIES since most recent `elpaca-process-queues'."
  (cl-remove-if (lambda (i) (time-less-p (get-text-property 0 'time (aref (cadr i) 3))
                                         elpaca--log-request-time))
                entries))

(defun elpaca-log--visit-byte-comp-warning (file line col)
  "Visit warning location in FILE at LINE and COL."
  (or (file-exists-p file) (user-error "File does not exist: %S" file))
  (find-file-other-window file)
  (goto-char (point-min))
  (forward-line (1- line))
  (move-to-column (1- col)))

(defun elpaca-log--byte-comp-warnings (entries)
  "Buttonize byte comp warnings in ENTRIES."
  (let ((queued (elpaca--queued)))
    (mapcar
     (lambda (entry)
       (if-let ((cols (cadr entry))
                ((equal (aref cols 1) "byte-compilation"))
                (copy (copy-tree entry))
                (info (string-trim (aref (cadr copy) 2)))
                (name (aref (cadr copy) 0))
                (e (elpaca-alist-get (intern name) queued)))
           (progn
             (when (string-match-p "\\(?:Error\\|Warning\\):" info)
               (setf (aref (cadr copy) 2) (propertize info 'face 'elpaca-log-error)))
             (when (string-match "\\(?:\\([^z-a]*?\\):\\([[:digit:]]+?\\):\\([[:digit:]]*?\\)\\):" info)
               (let ((file (match-string 1 info))
                     (line (match-string 2 info))
                     (col (match-string 3 info)))
                 (setf (aref (cadr copy) 2)
                       (replace-match
                        (elpaca-ui--buttonize
                         (propertize (string-join (list file col line) ":") 'face nil)
                         (lambda (&rest _)
                           (elpaca-log--visit-byte-comp-warning
                            (expand-file-name file (elpaca<-build-dir e))
                            (string-to-number line)
                            (string-to-number col))))
                        nil nil (aref (cadr copy) 2)))))
             copy)
         entry))
     entries)))

(defvar elpaca-log--follow-line nil)
(defun elpaca-log--follow ()
  "Display update diff if line has changed."
  (let ((line (line-number-at-pos)))
    (unless (equal elpaca-log--follow-line line)
      (setq elpaca-log--follow-line line)
      (ignore-errors (call-interactively #'elpaca-log-view-diff)))))

(define-minor-mode elpaca-log-update-mode "Auto display update diffs."
  :lighter " elum"
  (unless (derived-mode-p 'elpaca-log-mode) (user-error "Not in `elpaca-log-mode' buffer"))
  (if elpaca-log-update-mode
      (progn
        (add-hook 'post-command-hook #'elpaca-log--follow nil t)
        (elpaca-log--follow))
    (remove-hook 'post-command-hook #'elpaca-log--follow t)))

(declare-function magit-show-commit "magit")
(defun elpaca-log-magit-diff (id ref)
  "Show diff for ID at REF."
  (if-let ((fboundp 'magit-show-commit)
           (e (elpaca-get id))
           (default-directory (elpaca<-repo-dir e)))
      (let ((magit-display-buffer-noselect elpaca-log-update-mode)
            (magit-uniquify-buffer-names (not elpaca-log-update-mode))
            (magit-buffer-name-format "*elpaca-diff*"))
        (ignore magit-display-buffer-noselect magit-uniquify-buffer-names
                magit-buffer-name-format)
        (magit-show-commit ref))
    (user-error "Unable to show %s ref %s" id ref)))

(defun elpaca-log-diff (id ref)
  "Display diff buffer for package ID at REF."
  (if-let ((e (elpaca-get id))
           (repo (elpaca<-repo-dir e))
           (diff (let ((default-directory repo)) (elpaca-process-output "git" "show" ref))))
      (let ((displayp elpaca-log-update-mode))
        (with-current-buffer (get-buffer-create "*elpaca-diff*")
          (with-silent-modifications (erase-buffer) (insert diff))
          (diff-mode)
          (setq-local header-line-format (format "%s" id)
                      default-directory repo
                      diff-jump-to-old-file t)
          (funcall (if displayp #'display-buffer #'pop-to-buffer)
                   (current-buffer) '((display-buffer-reuse-window display-buffer-below-selected)))))
    (user-error "Unable to show diff for current revision")))

(defun elpaca-log-view-diff (data)
  "View commit diff for current log line's DATA."
  (interactive (list (save-excursion (goto-char (line-beginning-position))
                                     (save-restriction
                                       (narrow-to-region (point) (line-end-position))
                                       (condition-case _ (forward-button 1)
                                         (error (user-error "No ref found on current line")))
                                       (get-text-property (point) 'button-data)))))
  (funcall elpaca-log-diff-function (car data) (cdr data)))

(defun elpaca-log--updates (entries)
  "Return compact update log from ENTRIES."
  (cl-loop
   with compact
   for entry in entries
   do (if-let ((cols (cadr entry))
               (status (aref cols 1))
               ((equal status "update-log"))
               (info (aref cols 2))
               ((not (string-prefix-p "$git" info)))
               (tokens (split-string info " " 'omit-nulls))
               (commit (pop tokens))
               (date (propertize (replace-regexp-in-string "^.*\\((.*)\\)" "\\1" info)
                                 'face 'elpaca-log-info))
               (info (let* ((i (string-trim (replace-regexp-in-string (regexp-quote date) ""
                                                                      (string-join tokens " "))))
                            (i (replace-regexp-in-string "^\\* +" "" i))
                            (i (replace-regexp-in-string
                                "\\(?:[([]\\{1\\}[^z-a]*?#[^z-a]+?[])]\\{1\\}\\)"
                                (lambda (s) (propertize s 'face 'elpaca-log-highlight))
                                i)))
                       (replace-regexp-in-string
                        "^.*: " (lambda (s) (propertize s 'face 'elpaca-log-highlight))
                        i nil t)))
               (button (elpaca-ui--buttonize commit #'elpaca-log-view-diff
                                             (cons (caar entry) commit)))
               (copy (copy-tree entry)))
          (progn
            (setf (aref (cadr copy) 2) (concat button " " info " " date))
            (push copy compact))
        (when (string-match-p "failed" status) (push entry compact)))
   finally return compact))

(defun elpaca-log--verbosity (_ &optional limit)
  "Filter log entries according to `elpaca-verbosity' LIMIT."
  (let* ((elpaca-verbosity (or limit most-positive-fixnum))
         (elpaca-ui-search-query
          (replace-regexp-in-string
           "\\(?:#(?verbosity[^z-a]*?)\\|#verbosity\\)"
           ""
           (or (with-selected-window (minibuffer-window)
                 (when-let ((s (buffer-substring-no-properties (point-min) (point-max)))
                            ((string-match-p elpaca-ui-search-prompt s)))
                   (substring s (length elpaca-ui-search-prompt))))
               elpaca-ui-search-query))))
    (elpaca-ui-search-refresh (get-buffer elpaca-log-buffer) 'silent)
    tabulated-list-entries))

(defun elpaca-log--entries ()
  "Return log's `tabulated-list-entries'."
  (cl-loop
   with queue-time = (elpaca-q<-time (car (last elpaca--queues)))
   for (id . e) in (elpaca--queued)
   for log = (elpaca<-log e)
   for package = (elpaca<-package e)
   append
   (cl-loop
    for (status time info verbosity) in log
    for entry =
    (when-let (((<= verbosity elpaca-verbosity))
               (delta (format-time-string "%02s.%6N" (time-subtract time queue-time))))
      (list (list id) (vector (propertize package 'elpaca-status status)
                              (symbol-name status) info (propertize delta 'time time))))
    when entry collect entry)))

(defun elpaca-log--sort-chronologically (a b)
  "Sort entries A and B chronologically."
  (< (string-to-number (aref (cadr a) 3))
     (string-to-number (aref (cadr b) 3))))

(defvar elpaca-log-mode-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m elpaca-ui-mode-map)
    (define-key m (kbd "gd") 'elpaca-log-view-diff)
    (define-key m (kbd "gu") 'elpaca-log-updates)
    m))

(define-derived-mode elpaca-log-mode elpaca-ui-mode "elpaca-log-mode"
  "Major mode for displaying Elpaca order log entries."
  (setq tabulated-list-format [("Package" 30 t)
                               ("Status" 20 t)
                               ("Info" 80 t)
                               ("Time" 20 elpaca-log--sort-chronologically)]
        elpaca-ui--want-faces nil
        elpaca-ui-entries-function #'elpaca-log--entries
        elpaca-ui-header-line-prefix (propertize "Elpaca Log" 'face '(:weight bold))
        elpaca-ui-default-query elpaca-log-default-search-query
        elpaca-ui--history 'elpaca-log--history
        tabulated-list-use-header-line nil
        tabulated-list-sort-key '("Time"))
  (setq-local elpaca-ui-search-tags (append elpaca-ui-search-tags elpaca-log-search-tags))
  (tabulated-list-init-header))

;;;###autoload
(defun elpaca-log (&optional query interactive)
  "When INTERACTIVE is non-nil, Display `elpaca-log-buffer' filtered by QUERY.
Otherwise return log buffer string."
  (interactive (list nil t))
  (with-current-buffer (get-buffer-create elpaca-log-buffer)
    (unless (derived-mode-p 'elpaca-log-mode) (elpaca-log-mode))
    (elpaca-ui--update-search-query (current-buffer) (or query elpaca-ui-search-query))
    (if interactive (pop-to-buffer elpaca-log-buffer '((display-buffer-reuse-window display-buffer-same-window)))
      (buffer-substring-no-properties (save-excursion (goto-char (point-min)) (line-end-position)) (point-max)))))

;;;###autoload
(defun elpaca-log-updates ()
  "Log each available update without fetching."
  (interactive)
  (cl-loop for (_ . e) in (elpaca--queued) do (elpaca--log-updates e))
  (elpaca--maybe-log))

(provide 'elpaca-log)
;;; elpaca-log.el ends here
