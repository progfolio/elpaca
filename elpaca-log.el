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
    (latest    . (lambda (items) (butlast (reverse (sort (copy-tree items) #'elpaca-log--sort-chronologically))
                                          elpaca-ui--prev-entry-count)))
    (linked-errors . elpaca-log--byte-comp-warnings)
    (update-log . elpaca-log--updates))
  "Alist of search tags (see `elpaca-ui-search-tags') exclusive to the log buffer."
  :type '(alist :key-type symbol :value-type function) :group 'elpaca-ui)

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
   with (current compact)
   with buttonp = (fboundp 'magit-show-commit)
   for entry in (reverse entries)
   for cols = (cadr entry)
   for status = (aref cols 1)
   for info = (aref cols 2)
   do (if (and (equal status "update-log")
               (not (string-match-p "\\(?:^\\(?:\\(?:Author\\|Merge\\):\\)\\)" info)))
          (cond
           ((string-prefix-p "commit " info)
            (setf (alist-get 'commit current)
                  (if-let ((abbreviated  (substring info 7 13))
                           (buttonp))
                      (elpaca-ui--buttonize abbreviated #'elpaca-log--show-ref
                                            (cons (car entry) abbreviated))
                    (propertize abbreviated 'face 'elpaca-log-highlight))))
           ((string-prefix-p "Date: " info)
            (let ((ws (split-string (string-trim info) " ")))
              (setf (alist-get 'date current)
                    (format "(%s %s %s)" (nth 4 ws) (nth 5 ws) (nth 7 ws)))))
           (current ;; on first message line
            (let* ((copy (copy-tree entry))
                   (info (thread-last
                           (string-trim info)
                           (replace-regexp-in-string "^\\* +" "")
                           (replace-regexp-in-string
                            "\\(?:[([]\\{1\\}[^z-a]*?#[^z-a]+?[])]\\{1\\}\\)"
                            (lambda (s) (propertize s 'face 'elpaca-log-highlight)))
                           (replace-regexp-in-string
                            "^.*: " (lambda (s) (propertize s 'face 'elpaca-log-highlight))))))
              (setf (aref (cadr copy) 2)
                    (concat (alist-get 'commit current) " " info " "
                            (propertize (alist-get 'date current) 'face 'elpaca-log-info))
                    current nil)
              (push copy compact))))
        (when (string-match-p "failed" status) (push entry compact)))
   finally return compact))

(defun elpaca-log--verbosity (_ &optional limit)
  "Filter log entries according to `elpaca-verbosity' LIMIT."
  (let* ((elpaca-verbosity (or limit most-positive-fixnum))
         (elpaca-ui-search-filter
          (replace-regexp-in-string
           "\\(?:#(?verbosity[^z-a]*?)\\|#verbosity\\)"
           ""
           (or (with-selected-window (minibuffer-window)
                 (when-let ((s (buffer-substring-no-properties (point-min) (point-max)))
                            ((string-match-p elpaca-ui-search-prompt s)))
                   (substring s (length elpaca-ui-search-prompt))))
               elpaca-ui-search-filter))))
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
      (list item (vector pkg (symbol-name status) info delta)))
    when entry collect entry)))

;;;###autoload
(defun elpaca-log--latest (&rest queries)
  "Log latest activity with QUERIES."
  (elpaca-log)
  (with-current-buffer elpaca-log-buffer
    (setq elpaca-ui--prev-entry-count (length (funcall elpaca-ui-entries-function)))
    (elpaca-log (concat "#latest " (string-join queries " ")))))

(defun elpaca-log--sort-chronologically (a b)
  "Sort entries A and B chronologically."
  (< (string-to-number (aref (cadr a) 3))
     (string-to-number (aref (cadr b) 3))))

;;;###autoload
(defun elpaca-log (&optional filter)
  "Display `elpaca-log-buffer'.
If FILTER is non-nil, it is used as the initial search query."
  (interactive (list (when-let ((pkg (ignore-errors (elpaca-ui-current-package)))
                                (query (format "^%s$|" (symbol-name pkg)))
                                (quoted (regexp-quote query)))
                       (if (string-match-p quoted elpaca-ui-search-filter)
                           (replace-regexp-in-string quoted "" elpaca-ui-search-filter)
                         (concat (string-trim elpaca-ui-search-filter) " " query)))))
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
    (elpaca-ui--update-search-filter (current-buffer)
                                     (or filter elpaca-ui-search-filter))
    (pop-to-buffer elpaca-log-buffer '((display-buffer-reuse-window display-buffer-same-window)))))

;;;###autoload
(defun elpaca-status ()
  "Log most recent events for packages."
  (interactive)
  (with-current-buffer (elpaca-log)
    (let (elpaca-ui-want-tail) (elpaca-log "#unique"))))

(provide 'elpaca-log)
;;; elpaca-log.el ends here
