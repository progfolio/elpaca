;; Example Elpaca configuration -*- lexical-binding: t; -*-
(defvar elpaca-installer-version 0.14)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name (format "builds-%s/" emacs-version) elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-recipe '(elpaca) "Recipe for Elpaca itself.")
(let* ((bdir  (expand-file-name "elpaca-bootstrap/" elpaca-sources-directory))
       (emacs (concat invocation-directory invocation-name))
       (default-directory bdir))
  (unless (file-exists-p bdir)
    (make-directory bdir t)
    (with-current-buffer (get-buffer-create "*elpaca-bootstrap*")
      (condition-case err
          (if (and (zerop (call-process "git" nil t t "clone" "--depth=1" "--no-single-branch"
                                        "https://github.com/progfolio/elpaca.git" bdir))
                   (zerop (call-process "git" nil t nil "checkout" "feat/future"))
                   (zerop (call-process emacs nil t nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
              (kill-buffer)
            (pop-to-buffer (current-buffer))
            (error "Elpaca bootstrap failed"))
        ((error) (delete-directory bdir 'recursive) (signal (car err) (cdr err))))))
  (setq elpaca-installation
        (with-current-buffer (get-buffer-create "*elpaca-bootstrap*")
          (erase-buffer)
          (unless (zerop (call-process emacs nil t nil
                                       "-Q" "-L" bdir "--batch"
                                       "--eval" (format "(setq elpaca-directory %S
                                                                 elpaca-builds-directory %S
                                                                 elpaca-sources-directory %S)"
                                                        elpaca-directory
                                                        elpaca-builds-directory
                                                        elpaca-sources-directory)
                                       "--eval" "(require 'elpaca)"
                                       "--eval" (format "(princ (elpaca--bootstrap '%S))" elpaca-recipe)))
            (pop-to-buffer (current-buffer))
            (error "Elpaca bootstrap subprocess failed"))
          (prog1 (buffer-string) (kill-buffer)))))
(if (file-exists-p elpaca-installation)
    (add-to-list 'load-path elpaca-installation)
  (error "Malformed Elpaca bootstrap result: %S" elpaca-installation))
(require 'elpaca-autoloads)
(add-hook 'after-init-hook #'elpaca-process-queues)

;; Uncomment for systems which cannot create symlinks:
;; (elpaca-no-symlink-mode)

;; Install a package via the elpaca macro
;; See the "recipes" section of the manual for more details.

;; (elpaca example-package)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;;When installing a package used in the init file itself,
;;e.g. a package which adds a use-package key word,
;;use the :wait recipe keyword to block until that package is installed/configured.
;;For example:
;;(use-package general :ensure (:wait t) :demand t)

;; Expands to: (elpaca evil (use-package evil :demand t))
(use-package evil :ensure t :demand t)

;;Turns off elpaca-use-package-mode current declaration
;;Note this will cause evaluate the declaration immediately. It is not deferred.
;;Useful for configuring built-in emacs features.
(use-package emacs :ensure nil :config (setq ring-bell-function #'ignore))

;; Local Variables:
;; no-byte-compile: t
;; no-native-compile: t
;; no-update-autoloads: t
;; End:
