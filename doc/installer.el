;; Elpaca Installer. Copy below this line into your init.el -*- lexical-binding: t; -*-
(defvar elpaca-installer-version 0.14)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name (format "builds-%s/" emacs-version) elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-cache-directory (expand-file-name "cache/" elpaca-directory))
(defvar elpaca-recipe '(elpaca) "Recipe for Elpaca itself.")
(let* ((cache (expand-file-name "elpaca-bootstrap.eld" elpaca-cache-directory))
       (emacs (concat invocation-directory invocation-name))
       (slurp (lambda (f) (and (file-exists-p f) (with-temp-buffer (insert-file-contents f) (buffer-string)))))
       (calledp (lambda (&rest args) (zerop (apply #'call-process (car args) nil t nil (cdr args)))))
       (bdir (expand-file-name "elpaca-bootstrap/" elpaca-sources-directory))
       (default-directory bdir))
  (condition-case err
      (with-current-buffer (get-buffer-create "*elpaca-bootstrap*")
        (unless (file-exists-p (expand-file-name "elpaca.el"))
          (make-directory default-directory t)
          (pop-to-buffer-same-window (current-buffer))
          (unless (and (funcall calledp "git" "clone" "--depth=1" "--no-single-branch"
                                "https://github.com/progfolio/elpaca.git" bdir)
                       (funcall calledp "git" "checkout" "feat/future")
                       (funcall calledp emacs "-Q" "-L" "." "--batch"
                                "--eval" "(byte-recompile-directory \".\" 0 'force)"))
            (error "Elpaca bootstrap clone failed")))
        (add-to-list 'load-path
                     (or (funcall slurp cache)
                         (if (funcall calledp emacs
                                      "-Q" "-L" "." "--batch" "--eval"
                                      (format "(mapc (lambda (pair) (set (car pair) (cdr pair))) '%S)"
                                              (let (symbols)
                                                (mapatoms
                                                 (lambda (symbol)
                                                   (when (string-prefix-p "elpaca-" (symbol-name symbol))
                                                     (push (cons symbol (symbol-value symbol)) symbols))))
                                                (nreverse symbols)))
                                      "--eval" "(and (require 'elpaca) (elpaca--bootstrap elpaca-recipe))")
                             (prog1 (funcall slurp cache) (kill-buffer))
                           (error "Elpaca bootstrap subprocess failed")))))
    ((error) (when (file-exists-p bdir) (delete-directory bdir 'recursive))
     (signal (car err) (cdr err)))))
(require 'elpaca-autoloads)
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-recipe))
