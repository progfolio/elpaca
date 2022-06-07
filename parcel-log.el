;;; parcel-log.el --- Logging facilities for parcel.  -*- lexical-binding: t; -*-

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
(require 'parcel-ui)
(defvar parcel-log-buffer "*parcel-log*")

(defcustom parcel-log-default-search-query ".*"
  "Default query for `parcel-log-buffer'."
  :type 'string
  :group 'parcel)

(defun parcel-log--entries ()
  "Return log's `tabulated-list-entries'."
  (let ((queue-time (parcel-queue-time (car (last parcel--queues)))))
    (cl-loop
     for (item . order) in (parcel--queued-orders)
     for log = (parcel-order-log order)
     for package = (parcel-order-package order)
     append
     (cl-loop for (status time info) in log
              for delta = (format-time-string "%02s.%6N" (time-subtract time queue-time))
              for pkg = (propertize package 'face (parcel--status-face  status))
              collect (list item (vector pkg delta (symbol-name status) info))))))

;;;###autoload
(defun parcel-log (&optional _)
  "Display `parcel-log-buffer'."
  (interactive)
  (with-current-buffer (get-buffer-create parcel-log-buffer)
    (unless (derived-mode-p 'parcel-ui-mode)
      (parcel-ui-mode)
      (setq tabulated-list-format [("Package" 30 t)
                                   ("Time" 20 t)
                                   ("Status" 20 t)
                                   ("Info" 80 t)]
            parcel-ui-entries-function #'parcel-log--entries
            parcel-ui-header-line-prefix (propertize "Parcel Log" 'face '(:weight bold))
            tabulated-list-use-header-line nil
            tabulated-list-sort-key '("Time"))
      (tabulated-list-init-header)
      (parcel-ui--update-search-filter (current-buffer) parcel-log-default-search-query))
    (pop-to-buffer-same-window parcel-log-buffer)))

(provide 'parcel-log)
;;; parcel-log.el ends here
