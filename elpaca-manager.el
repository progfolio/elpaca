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
;;@TODO: do these need to be unconditionally required/executed?
(require 'bookmark)
(bookmark-maybe-load-default-file)

(defvar elpaca-manager-buffer "*elpaca-manager*")
(defvar-local elpaca-manager--entry-cache nil "Cache of all menu items.")

(defcustom elpaca-manager-default-search-query "#unique !#installed"
  "Default search query for `elpaca-manager'."
  :type 'string
  :group 'elpaca)

(defun elpaca-manager--entries (&optional recache)
  "Return list of all entries available in `elpaca-menu-functions' and init.
If RECACHE is non-nil, recompute `elpaca-manager--entry-cache'."
  (or (and (not recache) elpaca-manager--entry-cache)
      (setq elpaca-manager--entry-cache
             (cl-loop for (item . data) in (append (elpaca--menu-items)
                                                   (elpaca-ui--custom-candidates))
                      collect (list
                               item
                               (vector (symbol-name item)
                                       (or (plist-get data :description) "")
                                       (if-let ((date (plist-get data :date)))
                                           (format-time-string "%Y-%m-%d" date)
                                         "")
                                       (or (plist-get data :source) "")))))))

;;;###autoload
(defun elpaca-manager (&optional recache)
  "Display elpaca's package management UI.
If RECACHE is non-nil, recompute menu items from `elpaca-menu-item-functions'."
  (interactive "P")
  (when recache
    (elpaca--menu-items recache)
    (elpaca-manager--entries recache)
    (message "Elpaca menu item cache refreshed."))
  (with-current-buffer (get-buffer-create elpaca-manager-buffer)
    (unless (derived-mode-p 'elpaca-ui-mode)
      (elpaca-ui-mode)
      (setq tabulated-list-format [("Package" 30 t)
                                   ("Description" 80 t)
                                   ("Date" 15 t)
                                   ("Source" 20 t)]
            elpaca-ui-entries-function #'elpaca-manager--entries
            elpaca-ui-header-line-prefix (propertize "Elpaca Manager" 'face '(:weight bold))
            tabulated-list-use-header-line nil)
      (setq-local bookmark-make-record-function #'elpaca-manager-bookmark-make-record
                  elpaca-ui-default-query elpaca-manager-default-search-query)
      (tabulated-list-init-header)
      (elpaca-ui--update-search-filter (current-buffer) elpaca-manager-default-search-query))
    (pop-to-buffer-same-window elpaca-manager-buffer)))

;;;; Bookmark integration

;;;###autoload
(defun elpaca-manager--bookmark-handler (record)
  "Open a bookmarked search RECORD."
  (elpaca-manager)
  (pop-to-buffer-same-window elpaca-manager-buffer)
  (setq elpaca-ui-search-filter (bookmark-prop-get record 'query))
  (elpaca-ui-search-refresh))

(defun elpaca-manager-bookmark-make-record ()
  "Return a bookmark record for the current `elpaca-ui-search-filter'."
  (let ((name (replace-regexp-in-string
               " \(.* packages\):" ""
               (substring-no-properties (format-mode-line header-line-format)))))
    (list name
          (cons 'location name)
          (cons 'handler #'elpaca-manager--bookmark-handler)
          (cons 'query elpaca-ui-search-filter)
          (cons 'defaults nil))))


(provide 'elpaca-manager)
;;; elpaca-manager.el ends here
