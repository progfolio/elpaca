;;; elpaca-manager.el --- Elpaca package management UI  -*- lexical-binding: t; -*-

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

(defvar elpaca-manager-buffer "*elpaca-manager*")
(defvar elpaca-manager--history nil "`elpaca-manager' minibuffer history.")
(defvar-local elpaca-manager--entry-cache nil "Cache of all menu items.")

(defcustom elpaca-manager-default-search-query "#unique !#installed"
  "Default search query for `elpaca-manager'."
  :type 'string :group 'elpaca-ui)

(defun elpaca-manager--entries (&optional recache)
  "Return list of all entries available in `elpaca-menu-functions' and init.
If RECACHE is non-nil, recompute `elpaca-manager--entry-cache'."
  (or (and (not recache) elpaca-manager--entry-cache)
      (prog1 (setq elpaca-manager--entry-cache
                   (cl-loop
                    with queued = (elpaca--queued)
                    with init = (elpaca--custom-candidates t)
                    with try = (cl-set-difference (elpaca--custom-candidates) init :test #'equal)
                    with menus = (append (list (cons 'init-file init))
                                         (list (cons 'elpaca-try try))
                                         (elpaca--menu-items (or (and recache 'recache) t)))
                    for (menu . items) in menus append
                    (cl-loop
                     for (id . props) in items
                     for date = (or (when-let ((e (elpaca-alist-get id queued)))
                                      (elpaca--commit-date e "%Y-%m-%d"))
                                    (when-let ((declared (plist-get props :date)))
                                      (format-time-string "%F" declared)))
                     collect
                     (list (cons id menu)
                           (vector (symbol-name id)
                                   (or (plist-get props :description) "")
                                   (or date "")
                                   (propertize (or (plist-get props :source) "") 'menu menu))))))
        (when recache (message "Elpaca manager cache refreshed.")))))

(define-derived-mode elpaca-manager-mode elpaca-ui-mode "elpaca-manager-mode"
  "Major mode for displaying Elpaca packages."
  (setq tabulated-list-format [("Package" 30 elpaca-ui--sort-package)
                               ("Description" 80 t)
                               ("Date" 15 t)
                               ("Source" 20 t)]
        elpaca-ui-entries-function #'elpaca-manager--entries
        elpaca-ui-header-line-prefix (propertize "Elpaca Manager" 'face '(:weight bold))
        tabulated-list-use-header-line nil
        elpaca-ui--history 'elpaca-manager--history
        elpaca-ui-default-query elpaca-manager-default-search-query)
  (tabulated-list-init-header)
  (elpaca-ui--update-search-query (current-buffer) elpaca-ui-search-query))

;;;###autoload
(defun elpaca-manager (&optional recache)
  "Display Elpaca's package management UI.
If RECACHE is non-nil, recompute menu items from `elpaca-menu-functions'."
  (interactive "P")
  (with-current-buffer (get-buffer-create elpaca-manager-buffer)
    (unless (derived-mode-p 'elpaca-manager-mode) (elpaca-manager-mode))
    (when recache
      (elpaca-manager--entries recache)
      (elpaca-ui--update-search-query (current-buffer) elpaca-ui-search-query))
    (pop-to-buffer elpaca-manager-buffer '((display-buffer-reuse-window display-buffer-same-window)))))

(provide 'elpaca-manager)
;;; elpaca-manager.el ends here
