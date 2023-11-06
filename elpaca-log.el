;;; elpaca-log.el --- Logging facilities for elpaca.  -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Nicholas Vollmer

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

(defcustom elpaca-log-default-search-query ".*" "Default query for `elpaca-log-buffer'."
  :type 'string :group 'elpaca-ui)

(defcustom elpaca-log-search-tags
  '((verbosity . elpaca-log--verbosity)
    (latest . elpaca-log--tag-latest)
    (linked-errors . elpaca-log--byte-comp-warnings)
    (update-log . elpaca-log--updates))
  "Alist of search tags (see `elpaca-ui-search-tags') exclusive to the log buffer."
  :type '(alist :key-type symbol :value-type function) :group 'elpaca-ui)

(defcustom elpaca-log-command-queries
  '(((elpaca-fetch elpaca-fetch-all)   . "#latest #update-log")
    ((elpaca-try elpaca-rebuild)       . "#latest #linked-errors")
    ((elpaca-merge elpaca-merge-all) . "#unique | !finished")
    ((eval-buffer eval-region eval-defun eval-last-sexp org-ctrl-c-ctrl-c) . silent)
    (elpaca-delete . (lambda () (if (equal (buffer-name) elpaca-log-buffer)
                                    elpaca-ui-search-query 'silent)))
    (elpaca-ui-execute-marks . elpaca-log--marked-query))
  "Alist of form ((COMMAND-OR-COMMAND-LIST . QUERY-OR-FUNCTION)...).
If query is a string it is used when logging for that command.
If it is a function, it's return value is used."
  :type 'alist :group 'elpaca-ui)

(defun elpaca-log--marked-query ()
  "Return query for marked packages."
  (when (= (length (delete-dups (mapcar #'cadr elpaca-ui--marked-packages))) 1)
    (let ((this-command (cadar elpaca-ui--marked-packages))) (elpaca-log-defaults))))

(defun elpaca-log--find-command (val key)
  "Return t if KEY VAL."
  (or (eq key val) (and (listp val) (member key val))))

;;;###autoload
(defun elpaca-log-defaults ()
  "Return contextual logging queries."
  (if-let ((found (alist-get this-command elpaca-log-command-queries
                             nil nil #'elpaca-log--find-command))
           (result (if (functionp found) (funcall found) found)))
      result
    (if elpaca--ibs-set "#unique | !finished" "#latest")))

(defun elpaca-log--tag-latest (items)
  "Log latest ITEMS."
  (cl-remove-if (lambda (i) (time-less-p (get-text-property 0 'time (aref (cadr i) 3))
                                         elpaca--log-request-time))
                items))

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
                (info (aref (cadr copy) 2))
                (name (aref (cadr copy) 0))
                (e (elpaca-alist-get (intern name) queued)))
           (progn
             (when (string-match-p "\\(?:Error\\|Warning\\):" info)
               (setf (aref (cadr copy) 2) (propertize info 'face 'elpaca-log-error)))
             (when (string-match "\\(?:\\([^z-a]*?\\):\\([[:digit:]]+?\\):\\([[:digit:]]*?\\)\\):" info)
               (let ((file (match-string 1 (aref (cadr copy) 2)))
                     (line  (match-string 2 (aref (cadr copy) 2)))
                     (col (match-string 3 (aref (cadr copy) 2))))
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

(declare-function magit-show-commit "magit")
(defun elpaca-log--show-ref (data)
  "Show ref DATA."
  (if-let ((e (elpaca-get (car data)))
           (default-directory (elpaca<-repo-dir e)))
      (magit-show-commit (cdr data))
    (user-error "Unable to show ref at point")))

(defun elpaca-log--updates (entries)
  "Return compact update log from ENTRIES."
  (cl-loop
   with compact
   with buttonp = (fboundp 'magit-show-commit)
   for entry in entries
   do (if-let ((cols (cadr entry))
               (status (aref cols 1))
               ((equal status "update-log"))
               (info (aref cols 2))
               (tokens (split-string info " "))
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
               (button (if buttonp (elpaca-ui--buttonize commit #'elpaca-log--show-ref
                                                         (cons (car entry) commit))
                         commit))
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
   for (item . e) in (elpaca--queued)
   for log = (elpaca<-log e)
   for package = (elpaca<-package e)
   append
   (cl-loop
    for (status time info verbosity) in log
    for entry =
    (when-let
        (((<= verbosity elpaca-verbosity))
         (delta (format-time-string "%02s.%6N" (time-subtract time queue-time)))
         (pkg (let ((found (alist-get item elpaca-ui--string-cache)))
                (if-let ((cached (alist-get status found)))
                    cached
                  (setf (alist-get status (alist-get item elpaca-ui--string-cache))
                        (propertize
                         package 'face (elpaca-alist-get status elpaca-status-faces 'default)))))))
      (list item (vector pkg (symbol-name status) info (propertize delta 'time time))))
    when entry collect entry)))

(defun elpaca-log--sort-chronologically (a b)
  "Sort entries A and B chronologically."
  (< (string-to-number (aref (cadr a) 3))
     (string-to-number (aref (cadr b) 3))))

;;;###autoload
(defun elpaca-log (&optional query)
  "Display `elpaca-log-buffer' filtered by QUERY."
  (interactive (list (when-let ((pkg (ignore-errors (elpaca-ui-current-package)))
                                (query (format "^%s$|" (symbol-name pkg)))
                                (quoted (regexp-quote query)))
                       (if (string-match-p quoted elpaca-ui-search-query)
                           (replace-regexp-in-string quoted "" elpaca-ui-search-query)
                         (concat (string-trim elpaca-ui-search-query) " " query)))))
  (with-current-buffer (get-buffer-create elpaca-log-buffer)
    (unless (derived-mode-p 'elpaca-ui-mode)
      (elpaca-ui-mode)
      (setq tabulated-list-format [("Package" 30 t)
                                   ("Status" 20 t)
                                   ("Info" 80 t)
                                   ("Time" 20 elpaca-log--sort-chronologically)]
            elpaca-ui--want-faces nil
            elpaca-ui-want-tail t
            elpaca-ui-entries-function #'elpaca-log--entries
            elpaca-ui-header-line-prefix (propertize "Elpaca Log" 'face '(:weight bold))
            elpaca-ui-default-query elpaca-log-default-search-query
            elpaca-ui--history 'elpaca-log--history
            tabulated-list-use-header-line nil
            tabulated-list-sort-key '("Time"))
      (setq-local elpaca-ui-search-tags (append elpaca-ui-search-tags elpaca-log-search-tags))
      (tabulated-list-init-header))
    (elpaca-ui--update-search-query (current-buffer) (or query elpaca-ui-search-query))
    (pop-to-buffer elpaca-log-buffer '((display-buffer-reuse-window display-buffer-same-window)))))

;;;###autoload
(defun elpaca-status ()
  "Log most recent events for packages."
  (interactive)
  (with-current-buffer (elpaca-log)
    (let (elpaca-ui-want-tail)
      (elpaca-log (alist-get 'elpaca-status elpaca-log-command-queries "#unique")))))

(provide 'elpaca-log)
;;; elpaca-log.el ends here
