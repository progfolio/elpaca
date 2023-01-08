;;; elpaca-manager.el --- UI for elpaca package management  -*- lexical-binding: t; -*-

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

(defvar elpaca-manager-buffer "*elpaca-manager*")
(defvar elpaca-manager--history nil "`elpaca-manager' minibuffer history.")
(defvar-local elpaca-manager--entry-cache nil "Cache of all menu items.")

(defcustom elpaca-manager-default-search-query "#unique !#installed"
  "Default search query for `elpaca-manager'."
  :type 'string
  :group 'elpaca)

(defun elpaca-manager--entries (&optional recache)
  "Return list of all entries available in `elpaca-menu-functions' and init.
If RECACHE is non-nil, recompute `elpaca-manager--entry-cache'."
  (or (and (not recache) elpaca-manager--entry-cache)
      (prog1
          (setq elpaca-manager--entry-cache
                (cl-loop
                 for (item . data) in (append (elpaca--menu-items (or (and recache 'recache) t))
                                              (elpaca-ui--custom-candidates))
                 collect (list item
                               (vector (symbol-name item)
                                       (or (plist-get data :description) "")
                                       (or (ignore-errors (format-time-string
                                                           "%Y-%m-%d"
                                                           (plist-get data :date)))
                                           "")
                                       (or (plist-get data :source) "")))))
        (message "Elpaca menu item cache refreshed."))))

;;;###autoload
(defun elpaca-manager (&optional recache)
  "Display elpaca's package management UI.
If RECACHE is non-nil, recompute menu items from `elpaca-menu-item-functions'."
  (interactive "P")
  (when recache (elpaca-manager--entries recache))
  (with-current-buffer (get-buffer-create elpaca-manager-buffer)
    (let ((initializedp (derived-mode-p 'elpaca-ui-mode)))
      (unless initializedp
        (elpaca-ui-mode)
        (setq tabulated-list-format [("Package" 30 t)
                                     ("Description" 80 t)
                                     ("Date" 15 t)
                                     ("Source" 20 t)]
              elpaca-ui-entries-function #'elpaca-manager--entries
              elpaca-ui-header-line-prefix (propertize "Elpaca Manager" 'face '(:weight bold))
              elpaca-ui-want-tail nil
              tabulated-list-use-header-line nil
              elpaca-ui--history 'elpaca-manager--history
              elpaca-ui-default-query elpaca-manager-default-search-query)
        (tabulated-list-init-header))
      (when (or (not initializedp) recache)
        (elpaca-ui--update-search-filter (current-buffer) elpaca-ui-search-filter))))
  (pop-to-buffer elpaca-manager-buffer '((display-buffer-reuse-window display-buffer-same-window))))

(provide 'elpaca-manager)
;;; elpaca-manager.el ends here
