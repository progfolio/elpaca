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

(defcustom elpaca-log-default-search-query ".*"
  "Default query for `elpaca-log-buffer'."
  :type 'string
  :group 'elpaca)

(defun elpaca-log--entries ()
  "Return log's `tabulated-list-entries'."
  (cl-loop with queue-time = (elpaca-q<-time (car (last elpaca--queues)))
           for (item . p) in (elpaca--queued)
           for log = (elpaca<-log p)
           for package = (elpaca<-package p)
           append
           (cl-loop for (status time info) in log
                    for delta = (format-time-string "%02s.%6N" (time-subtract time queue-time))
                    for pkg = (propertize package 'face (elpaca--status-face status) 'elpaca p)
                    collect (list item (vector pkg (symbol-name status) info delta)))))

;;;###autoload
(defun elpaca-log (&optional filter)
  "Display `elpaca-log-buffer'.
If FILTER is non-nil, it is used as the initial search query."
  (interactive (list (when-let ((e (get-text-property (line-beginning-position) 'elpaca))
                                (package (elpaca<-package e)))
                       (format "^%s$|" package))))
  (with-current-buffer (get-buffer-create elpaca-log-buffer)
    (unless (derived-mode-p 'elpaca-ui-mode)
      (elpaca-ui-mode)
      (setq tabulated-list-format [("Package" 30 t)
                                   ("Status" 20 t)
                                   ("Info" 40 t)
                                   ("Time" 20 t)]
            elpaca-ui--want-faces nil
            elpaca-ui-entries-function #'elpaca-log--entries
            elpaca-ui-header-line-prefix (propertize "Elpaca Log" 'face '(:weight bold))
            tabulated-list-use-header-line nil
            tabulated-list-sort-key '("Time"))
      (tabulated-list-init-header))
    (elpaca-ui--update-search-filter (current-buffer)
                                     (or filter elpaca-log-default-search-query))
    (pop-to-buffer-same-window elpaca-log-buffer)))

(provide 'elpaca-log)
;;; elpaca-log.el ends here
