;; Example Elpaca configuration -*- lexical-binding: t; -*-
(defvar elpaca-installer-version 0.14)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name (format "builds-%s/" emacs-version) elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-cache-directory (expand-file-name "cache/" elpaca-directory))
(defvar elpaca-recipe '(elpaca) "Recipe for Elpaca itself.")
(let* ((cache (expand-file-name "elpaca-bootstrap.eld" elpaca-cache-directory))
       (emacs (concat invocation-directory invocation-name))
       (slurp (lambda (f) (and (file-exists-p f) (with-temp-buffer (insert-file-contents f) (read (current-buffer))))))
       (calledp (lambda (&rest args) (zerop (apply #'call-process (car args) nil t t (cdr args)))))
       (bdir (expand-file-name "elpaca-bootstrap/" elpaca-sources-directory))
       (default-directory bdir)
       (env (let (symbols)
              (mapatoms (lambda (symbol)
                          (when (string-prefix-p "elpaca-" (symbol-name symbol))
                            (push (cons symbol (symbol-value symbol)) symbols))))
              (sort symbols (lambda (a b) (string< (car a) (car b))))))
       (env-hash (secure-hash 'md5 (format "%S" env)))
       (cached (funcall slurp cache))
       (build (and cached (string= (car cached) env-hash) (cadr cached))))
  (condition-case err
      (with-current-buffer (get-buffer-create "*elpaca-bootstrap*")
        (unless (file-exists-p (expand-file-name "elpaca.el"))
          (make-directory default-directory t)
          (pop-to-buffer-same-window (current-buffer))
          (unless (and (funcall calledp "git" "clone" "--depth=1" "--no-single-branch"
                                "https://github.com/progfolio/elpaca.git" bdir)
                       (funcall calledp emacs "-Q" "-L" "." "--batch"
                                "--eval" "(byte-recompile-directory \".\" 0 'force)"))
            (error "Elpaca bootstrap clone failed")))
        (add-to-list 'load-path
                     (or (and build (file-exists-p build) build)
                         (if (funcall calledp emacs
                                      "-Q" "-L" "." "--batch" "--eval"
                                      (format "(mapc (lambda (pair) (set (car pair) (cdr pair))) '%S)" env)
                                      "--eval" (format "(and (require 'elpaca) (elpaca--bootstrap elpaca-recipe %S))" env-hash))
                             (cadr (funcall slurp cache))
                           (error "Elpaca bootstrap subprocess failed"))))
        (kill-buffer))
    ((error) (when (file-exists-p bdir) (delete-directory bdir 'recursive))
     (signal (car err) (cdr err)))))
(require 'elpaca-autoloads)
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-recipe))

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
