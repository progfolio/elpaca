;;; elpaca-suggest.el --- Minor mode for suggesting packages  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Nicholas Vollmer

;; Author: Nicholas Vollmer
;; Keywords: convenience
;; Package-Requires: ((emacs "27.1") (elpaca "0"))
;; Version: 0.0.0

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
(require 'elpaca)
(require 'elpaca-manager)

(defvar elpaca-suggest--db
  (let ((dir (file-name-directory (or load-file-name (buffer-file-name)))))
    (elpaca--read-file (expand-file-name "./elpaca-suggest-db.eld" dir))))
(defvar elpaca-suggest-auto-mode-alist )

(defun elpaca-suggest--activate-installed ()
  "Activate fundamental mode buffers after installing packages."
  (remove-hook 'elpaca--post-queues-hook #'elpaca-suggest--activate-installed)
  (remove-hook 'elpaca-ui--pre-execute-marks-hook #'elpaca-suggest--activate-installed)
  (cl-loop for buffer in (buffer-list) do
           (with-current-buffer buffer (when (eq major-mode 'fundamental-mode) (normal-mode)))))

(defun elpaca-suggest (query)
  "Suggest major modes for QUERY." ;;@TODO: restore query, kill buffer if we made it
  (with-current-buffer (elpaca-manager nil 'nodisplay)
    (elpaca-ui-search query)
    (let ((entries elpaca-ui-entries))
      (if (= (length entries) 0)
          (message "No results for: %s" query)
        (add-hook 'elpaca-ui--pre-execute-marks-hook
                  (lambda () (add-hook 'elpaca--post-queues-hook #'elpaca-suggest--activate-installed))
                  nil 'local)
        (pop-to-buffer (current-buffer))))))

(defun elpaca-suggest--query-db (key string test)
  "Return list of queries matching STRING for DB KEY.
TEST is a function called with STRING REGEXP SYM."
  (cl-loop with seen for (regexp . sym) in (alist-get key elpaca-suggest--db)
           when (and (not (member sym seen))
                     (stringp regexp)
                     (funcall test regexp string))
           collect (and (push sym seen) (symbol-name sym))))

(defun elpaca-suggest--first-line (&optional buffer)
  "Return BUFFER's first line."
  (with-current-buffer (or buffer (current-buffer))
    (save-restriction (widen)
                      (let ((min (point-min)))
                        (buffer-substring-no-properties
                         min (save-excursion (goto-char min) (line-end-position)))))))

;;;###autoload
(defun elpaca-suggest-buffer-major-mode (buffer)
  "Suggest major mode for BUFFER."
  (interactive (list (current-buffer)))
  (if-let ((first (elpaca-suggest--first-line buffer))
           (query (append
                   (elpaca-suggest--query-db 'auto-mode-alist (buffer-name buffer) #'string-match-p)
                   (elpaca-suggest--query-db 'magic-mode-alist first #'string-match-p)
                   (and-let* (((string-match auto-mode-interpreter-regexp first))
                              (match (match-string 2 first))
                              (file (format "\\`%s\\'" (file-name-nondirectory match))))
                     (elpaca-suggest--query-db 'interpreter-mode-alist file #'string-match-p)))))
      (elpaca-suggest (concat "#unique !#installed ^" (string-join query "\\\\|") " |"))
    (message "No suggestions for current buffer")))

(defun elpaca-suggest--fundamental-buffers ()
  "Suggest major mode for buffers in `fundamental-mode'."
  (when (eq major-mode 'fundamental-mode)
    (run-with-idle-timer 0 nil #'elpaca-suggest-buffer-major-mode (current-buffer))))

;;;###autoload
(define-minor-mode elpaca-suggest-mode "Automatically suggest packages for buffers."
  :global t :group 'elpaca
  (funcall (if elpaca-suggest-mode #'add-hook #'remove-hook)
           'after-change-major-mode-hook #'elpaca-suggest--fundamental-buffers))

(provide 'elpaca-suggest)
;;; elpaca-suggest.el ends here
