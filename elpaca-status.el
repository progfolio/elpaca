;;; elpaca-status.el --- Elpaca status buffer view.   -*- lexical-binding: t; -*-

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
(require 'elpaca-ui)

(defconst elpaca-status-buffer "*elpaca-status*")

(defcustom elpaca-status-default-search-query ".*"
  "Default search query for elpaca status buffer."
  :type  'string
  :group 'elpaca)

(defcustom elpaca-status-auto-kill t
  "When non-nil, the status buffer is killed after successfully processed queues."
  :type 'boolean
  :group 'elpaca)

(defun elpaca-status--entries (&optional queued)
  "Return list of `tabultaed-list-entries' from QUEUED E's."
  (cl-loop for (item . e) in (reverse (or queued
                                          (elpaca-q<-elpacas
                                           (cl-find-if #'elpaca-q<-elpacas elpaca--queues))))
           for status = (elpaca--status e)
           collect
           (list item (vector (propertize (elpaca<-package e)
                                          'face (elpaca--status-face status)
                                          'elpaca e)
                              (symbol-name status)
                              (elpaca--info e)))))

(defun elpaca-status--header-line (&optional queued)
  "Set `elpaca-buffer' header line to reflect QUEUED Elpaca statuses."
  (let* ((queued (or queued (elpaca-q<-elpacas
                             (cl-find-if #'elpaca-q<-elpacas elpaca--queues))))
         (counts nil)
         (queue-len (length queued)))
    (dolist (q queued)
      (let ((status (elpaca--status (cdr q))))
        (if (elpaca-alist-get status counts)
            ;; Avoid `elpaca-alist-get'. doesn't return PLACE.
            (cl-incf (alist-get status counts))
          (push (cons status 1) counts))))
    (with-current-buffer (get-buffer-create elpaca-status-buffer)
      (setq header-line-format
            (concat
             (propertize " Elpaca " 'face '(:weight bold))
             " "
             (format "Queued: %d | %s(%.2f%%%%): %d | %s: %d | %s: %d"
                     queue-len
                     (propertize "Finished" 'face 'elpaca-finished)
                     (if-let ((finished (elpaca-alist-get 'finished counts)))
                         (* (/ (float finished) queue-len) 100)
                       0.00)
                     (or (elpaca-alist-get 'finished counts) 0)
                     (propertize "Blocked" 'face 'elpaca-blocked)
                     (or (elpaca-alist-get 'blocked  counts) 0)
                     (propertize "Failed" 'face 'elpaca-failed)
                     (or (elpaca-alist-get 'failed   counts) 0)))))))

;;;###autoload
(defun elpaca-status (&optional all noselect)
  "Diplay `elpaca-status-buffer' for latest queue.
When ALL is non-nil display all queues, else, display only the most recent.
If NOSELECT is non-nil, do not make the status buffer current."
  (interactive "P")
  (with-current-buffer (get-buffer-create elpaca-status-buffer)
    (unless (derived-mode-p 'elpaca-ui-mode)
      (elpaca-ui-mode)
      (setq elpaca-ui-entries-function
            (if all
                (lambda (&optional _)
                  (elpaca-status--entries
                   (cl-loop for q in elpaca--queues append (elpaca-q<-elpacas q))))
              #'elpaca-status--entries))
      (setq tabulated-list-format [("Package" 30 t) ("Status" 15 t) ("Info" 100 t)]
            tabulated-list-entries #'elpaca-status--entries
            elpaca-ui-header-line-prefix (propertize "Elpaca Status" 'face '(:weight bold))
            tabulated-list-use-header-line nil
            header-line-format (elpaca-status--header-line)))
    (tabulated-list-init-header)
    (tabulated-list-print 'remember-pos)
    (unless noselect (pop-to-buffer-same-window elpaca-status-buffer))))

(provide 'elpaca-status)
;;; elpaca-status.el ends here
