;;; parcel-status.el --- Parcel status buffer view.   -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Nicholas Vollmer

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
(require 'cl-lib)
(require 'parcel-ui)

(defconst parcel-status-buffer "*parcel-status*")
(defcustom parcel-status-default-search-query ".*"
  "Default search query for parcel status buffer."
  :type  'string
  :group 'parcel)

(defcustom parcel-status-auto-kill t
  "When non-nil, the status buffer is killed after successfully processed queues."
  :type 'boolean
  :group 'parcel)

(defun parcel-status--entries (&optional queued)
  "Return list of `tabultaed-list-entries' from QUEUED orders."
  (cl-loop for (item . order) in (reverse
                                  (or queued
                                      (parcel-queue-orders
                                       (cl-find-if #'parcel-queue-orders parcel--queues))))
           for status = (parcel-order-status order)
           collect
           (list item (vector (propertize (parcel-order-package order)
                                          'face (parcel--status-face status)
                                          'order order)
                              (symbol-name status)
                              (parcel-order-info order)))))

(defun parcel-status--header-line (&optional queued)
  "Set `parcel-buffer' header line to reflect QUEUED order statuses."
  (let* ((queued (or queued (parcel-queue-orders
                             (cl-find-if #'parcel-queue-orders parcel--queues))))
         (counts nil)
         (queue-len (length queued)))
    (dolist (q queued)
      (let ((status (parcel-order-status (cdr q))))
        (if (parcel-alist-get status counts)
            ;; Avoid `parcel-alist-get'. doesn't return PLACE.
            (cl-incf (alist-get status counts))
          (push (cons status 1) counts))))
    (with-current-buffer (get-buffer-create parcel-status-buffer)
      (setq header-line-format
            (concat
             (propertize " Parcel " 'face '(:weight bold))
             " "
             (format "Queued: %d | %s(%.2f%%%%): %d | %s: %d | %s: %d"
                     queue-len
                     (propertize "Finished" 'face 'parcel-finished)
                     (if-let ((finished (parcel-alist-get 'finished counts)))
                         (* (/ (float finished) queue-len) 100)
                       0.00)
                     (or (parcel-alist-get 'finished counts) 0)
                     (propertize "Blocked" 'face 'parcel-blocked)
                     (or (parcel-alist-get 'blocked  counts) 0)
                     (propertize "Failed" 'face 'parcel-failed)
                     (or (parcel-alist-get 'failed   counts) 0)))))))

;;;###autoload
(defun parcel-status (&optional all noselect)
  "Diplay `parcel-status-buffer' for latest queue.
When ALL is non-nil display all queues, else, display only the most recent.
If NOSELECT is non-nil, do not make the status buffer current."
  (interactive "P")
  (with-current-buffer (get-buffer-create parcel-status-buffer)
    (unless (derived-mode-p 'parcel-ui-mode)
      (parcel-ui-mode)
      (setq parcel-ui-entries-function
            (if all
                (lambda (&optional _)
                  (parcel-status--entries
                   (apply #'append (cl-loop for queue in parcel--queues
                                            collect (parcel-queue-orders queue)))))
              #'parcel-status--entries))
      (setq tabulated-list-format [("Package" 30 t) ("Status" 15 t) ("Info" 100 t)]
            tabulated-list-entries #'parcel-status--entries
            parcel-ui-header-line-prefix (propertize "Parcel Status" 'face '(:weight bold))
            tabulated-list-use-header-line nil
            header-line-format (parcel-status--header-line)))
    (tabulated-list-init-header)
    (tabulated-list-print 'remember-pos)
    (unless noselect (pop-to-buffer-same-window parcel-status-buffer))))

(defvar parcel-status-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "I") 'parcel-status-mode-send-input)
    (define-key map (kbd "R") 'parcel-status-mode-visit-repo)
    (define-key map (kbd "B") 'parcel-status-mode-visit-build)
    (define-key map (kbd "D") 'parcel-status-delete-package)
    map))

(define-derived-mode parcel-status-mode parcel-ui-mode "Parcel Status Mode"
  "Mode for interacting with the parcel status buffer.

  \\{parcel-status-mode-map}")

(defun parcel-status-mode-send-input ()
  "Send input string to current process."
  (interactive)
  (if-let ((order (get-text-property (line-beginning-position) 'order))
           (process (parcel-order-process order))
           ((process-live-p process)))
      (let* ((input (read-string (format "Send input to %S: " (process-name process)))))
        (process-send-string process (concat input "\n")))
    (user-error "No running process associated with %S" (parcel-order-package order))))

(defun parcel-status-mode--visit (type)
  "Visit current order's TYPE dir.
TYPE is either the symbol `repo` or `build`."
  (if-let ((order (get-text-property (line-beginning-position) 'order))
           (dir   (funcall (intern (format "parcel-order-%s-dir" type)) order))
           ((file-exists-p dir)))
      (dired dir)
    (user-error "No %s dir associated with current line" type)))

(defun parcel-status-mode-visit-repo ()
  "Visit repo associated with current process."
  (interactive)
  (parcel-status-mode--visit 'repo))

(defun parcel-status-mode-visit-build ()
  "Visit builds dir associated with current process."
  (interactive)
  (parcel-status-mode--visit 'build))

(defun parcel-status-delete-package (&optional force)
  "Delete package at point. If FORCE is non-nil, do not confirm."
  (interactive)
  (if-let ((package (intern (parcel-order-package
                             (get-text-property (line-beginning-position) 'order)))))
      (parcel-delete-package force nil package)))

(provide 'parcel-status)
;;; parcel-status.el ends here
