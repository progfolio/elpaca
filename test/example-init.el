;;; init-elpaca.el --- ELPACA INIT DEMO   -*- lexical-binding: t; -*-

;; Bootstrap Elpaca
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :build (:not elpaca--activate-package)))
(when-let ((repo  (expand-file-name "repos/elpaca/" elpaca-directory))
           (build (expand-file-name "elpaca/" elpaca-builds-directory))
           (order (cdr elpaca-order))
           ((add-to-list 'load-path (if (file-exists-p build) build repo)))
           ((not (file-exists-p repo)))
           (buffer (get-buffer-create "*elpaca-bootstrap*")))
  (condition-case-unless-debug err
      (if-let (((pop-to-buffer buffer '((display-buffer-reuse-window
                                         display-buffer-same-window))))
               ((zerop (call-process "git" nil buffer t "clone"
                                     (plist-get order :repo) repo)))
               (default-directory repo)
               ((zerop (call-process "git" nil buffer t "checkout"
                                     (or (plist-get order :ref) "--")))))
          (progn
            (byte-recompile-directory repo 0 'force)
            (require 'elpaca)
            (and (fboundp 'elpaca-generate-autoloads)
                 (elpaca-generate-autoloads "elpaca" repo))
            (kill-buffer buffer))
        (error "%s" (with-current-buffer buffer (buffer-string))))
    ((error)
     (warn "%s" err)
     (delete-directory repo 'recursive))))
(require 'elpaca-autoloads)
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package
(elpaca use-package (require 'use-package))
;; Install evil and configure via `use-package'
(elpaca-use-package evil :demand t)
;; Install a package from a user-provided recipe
(elpaca (yodel :host github :repo "progfolio/yodel"))

;; Local Variables:
;; no-byte-compile: t
;; End:
