;;; elpaca-log.el --- Logging facilities for elpaca.  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Nicholas Vollmer

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

(defcustom elpaca-log-default-search-query ".*"
  "Default query for `elpaca-log-buffer'."
  :type 'string
  :group 'elpaca)

(defun elpaca-log-verbosity (items &optional limit)
  (if (not (eq (current-buffer) (get-buffer elpaca-log-buffer)))
      (message "#verbosity tag only applicable in elpaca-log-buffer")
    (unless (= limit elpaca-verbosity) (setq-local elpaca-verbosity limit)
            (run-at-time 0 nil (lambda (b) (with-current-buffer b (elpaca-ui-search-refresh)))
                         (current-buffer))))
  items)

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

(defun elpaca-log--build-entries (entries)
  "Return a list of ENTRIES filtered to last builds."
  (cl-loop with ids = (delete-dups (mapcar #'car entries))
           with queue-time = (elpaca-q<-time (car (last elpaca--queues)))
           with queued = (elpaca--queued)
           for id in ids
           for package = (symbol-name id)
           for e = (alist-get id queued)
           for log = (elpaca<-log e)
           for events =
           (cl-loop
            for (status time info) in log
            until (eq status 'rebuilding)
            for pkg = (let ((found (alist-get id elpaca-ui--string-cache)))
                        (if-let ((cached (alist-get status found)))
                            cached
                          (setf (alist-get status (alist-get id elpaca-ui--string-cache))
                                (propertize package 'face (elpaca-alist-get status elpaca-status-faces 'default)))))
            collect
            (list id (vector pkg (symbol-name status) info
                             (format-time-string
                              "%02s.%6N" (time-subtract time queue-time)))))
           unless (eq (length events) (length log))
           append events))

;;;###autoload
(defun elpaca-log--latest ()
  "Log latest activity."
  (elpaca-log)
  (with-current-buffer elpaca-log-buffer
    (setq elpaca-ui--prev-entry-count (length (funcall elpaca-ui-entries-function)))
    (elpaca-log "#latest #linked-errors")))

(defun elpaca-log--sort-chronologically (a b)
  "Sort entries A and B chronologically."
  (< (string-to-number (aref (cadr a) 3))
     (string-to-number (aref (cadr b) 3))))

;;;###autoload
(defun elpaca-log (&optional filter)
  "Display `elpaca-log-buffer'.
If FILTER is non-nil, it is used as the initial search query."
  (interactive (list (when-let ((pkg (ignore-errors (elpaca-ui-current-package)))
                                ((alist-get pkg (elpaca--queued))))
                       (format "^%s$|" (symbol-name pkg)))))
  (with-current-buffer (get-buffer-create elpaca-log-buffer)
    (unless (derived-mode-p 'elpaca-ui-mode)
      (elpaca-ui-mode)
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
      (tabulated-list-init-header))
    (elpaca-ui--update-search-filter (current-buffer)
                                     (or filter elpaca-ui-search-filter))
    (pop-to-buffer elpaca-log-buffer '((display-buffer-reuse-window display-buffer-same-window)))))

;;;###autoload
(defun elpaca-status ()
  "Log most recent events for packages."
  (interactive)
  (elpaca-log "#unique"))

(provide 'elpaca-log)
;;; elpaca-log.el ends here
