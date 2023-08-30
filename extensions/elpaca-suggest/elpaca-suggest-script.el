;; -*- lexical-binding: t; -*-
;;Store locally so subsequent runs only have to download new major modes.
(defvar elpaca-suggest-script-db-path  "~/Documents/elpaca-suggest-db/")
(defvar elpaca-suggest-script-db-file  (expand-file-name "./elpaca-suggest-db.eld"))
(eval
 `(elpaca-test ;; eval to regenerate init.el in script env and run script.
    :interactive t
    :keep t
    :dir ,elpaca-suggest-script-db-path
    :early-init
    (load-theme 'modus-vivendi t)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (menu-bar-mode -1)
    :init
    (setq initial-buffer-choice t
          inhibit-startup-screen t
          inhibit-startup-message t)
    (defvar default-auto-mode-alist auto-mode-alist)
    (defvar default-magic-mode-alist magic-mode-alist)
    (defvar default-interpreter-mode-alist interpreter-mode-alist)
    (setq elpaca-queue-limit 10 ; Throttle so we don't hit any ELPA API limits
          ;; We're only interested in Autoloads
          elpaca-build-steps '( elpaca--clone elpaca--configure-remotes elpaca--checkout-ref
                                ;;elpaca--clone-dependencies @Maybe?
                                elpaca--link-build-files elpaca--generate-autoloads-async
                                elpaca--activate-package))

    (elpaca-update-menus) ; Update menus to include new major modes.

    (message "Installing Major Modes...")
    (with-current-buffer (elpaca-manager t)
      (elpaca-ui-search "#unique major mode")
      (mapc #'elpaca-try (mapcar #'car tabulated-list-entries)))

    (add-hook
     'elpaca-after-init-hook
     (lambda ()
       (message "Installing Major Modes...done")
       (message "Saving variable database...")
       (elpaca--write-file ,elpaca-suggest-script-db-file
         (print (cl-loop for name in '("auto" "interpreter" "magic")
                         for varname = (concat name "-mode-alist")
                         for loaded = (intern varname)
                         for defaults = (intern (concat "default-" varname))
                         collect (cons loaded
                                       (cl-sort (cl-set-difference (symbol-value loaded)
                                                                   (symbol-value defaults)
                                                                   :test #'equal)
                                                #'string< :key (lambda (cell) (if (consp (cdr cell))
                                                                                  (cadr cell)
                                                                                (cdr cell)))))))

         (goto-char (point-min))
         (save-excursion (while (re-search-forward "\\(?:) \\)" nil 'noerror) (replace-match ")\n")))
         (while (re-search-forward "\\((\\(?:auto\\|interpreter\\|magic\\)-mode-alist\\)"
                                   nil 'noerror)
           (insert "\n"))
         (lisp-data-mode)
         (indent-region (point-min) (point-max)))
       (message "Saving variable database...done"))))
 t)

;; Local Variables:
;; no-byte-compile: t
;; End:
