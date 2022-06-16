;;; parcel-manager.el --- UI for parcel package management  -*- lexical-binding: t; -*-

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
;;@TODO: do these need to be unconditionally required/executed?
(require 'bookmark)
(bookmark-maybe-load-default-file)

(defvar parcel-manager-buffer "*parcel-manager*")
(defvar-local parcel-manager--entry-cache nil "Cache of all menu items.")

(defcustom parcel-manager-default-search-query ".*"
  "Default search query for `parcel-manager'."
  :type 'string
  :group 'parcel)

(defun parcel-manager--entries (&optional recache)
  "Return list of all entries available in `parcel-menu-functions' and init.
If RECACHE is non-nil, recompute `parcel-manager--entry-cache'."
  (or (and (not recache) parcel-manager--entry-cache)
      (let ((queued (parcel--queued-orders)))
        (setq parcel-manager--entry-cache
              (reverse
               (cl-loop for (item . data) in (reverse (append (parcel-menu--candidates)
                                                              (parcel-ui--custom-candidates)))
                        collect (list
                                 item
                                 (vector (propertize (format "%S" item) 'order (alist-get item queued))
                                         (or (plist-get data :description) "")
                                         (if-let ((date (plist-get data :date)))
                                             (format-time-string "%Y-%m-%d" date)
                                           "")
                                         (or (plist-get data :source) "")))))))))

;;;###autoload
(defun parcel-manager (&optional recache)
  "Display parcel's package management UI.
If RECACHE is non-nil, recompute menu items from `parcel-menu-item-functions'."
  (interactive "P")
  (when recache
    (parcel-menu--candidates recache)
    (parcel-manager--entries recache))
  (with-current-buffer (get-buffer-create parcel-manager-buffer)
    (unless (derived-mode-p 'parcel-ui-mode)
      (parcel-ui-mode)
      (setq tabulated-list-format [("Package" 30 t)
                                   ("Description" 80 t)
                                   ("Date" 15 t)
                                   ("Source" 20 t)]
            parcel-ui-entries-function #'parcel-manager--entries
            parcel-ui-header-line-prefix (propertize "Parcel Manager" 'face '(:weight bold))
            tabulated-list-use-header-line nil)
      (setq-local bookmark-make-record-function #'parcel-manager-bookmark-make-record)
      (tabulated-list-init-header)
      (parcel-ui--update-search-filter (current-buffer) parcel-manager-default-search-query))
    (pop-to-buffer-same-window parcel-manager-buffer)))

;;;; Bookmark integration

;;;###autoload
(defun parcel-manager--bookmark-handler (record)
  "Open a bookmarked search RECORD."
  (parcel-manager)
  (pop-to-buffer-same-window parcel-manager-buffer)
  (setq parcel-ui-search-filter (bookmark-prop-get record 'query))
  (parcel-ui-search-refresh))

(defun parcel-manager-bookmark-make-record ()
  "Return a bookmark record for the current `parcel-ui-search-filter'."
  (let ((name (replace-regexp-in-string
               " \(.* packages\):" ""
               (substring-no-properties (format-mode-line header-line-format)))))
    (list name
          (cons 'location name)
          (cons 'handler #'parcel-manager--bookmark-handler)
          (cons 'query parcel-ui-search-filter)
          (cons 'defaults nil))))


(provide 'parcel-manager)
;;; parcel-manager.el ends here
