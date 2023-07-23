;;; init-dev.el --- development settings for init file  -*- lexical-binding: t; -*-


;;; Commentary:
;; Evaluated when loading init file.
;; Cleaner to keep this in a separate file rather than a long single line at the top of init.org

;;; Code:
(setq-local org-confirm-babel-evaluate nil)
(require 'auto-tangle-mode)
(auto-tangle-mode)
(add-hook 'auto-tangle-after-tangle-hook (lambda ()
                                           (let ((elpaca-log-functions nil))
                                             (load-file "~/.emacs.d/init.el")
                                             (elpaca-process-queues))))
(eldoc-mode)
(setq ispell-buffer-session-localwords '( "ELPA"
                                          "EPUB"
                                          "Elisp"
                                          "GC"
                                          "LocalWords"
                                          "MELPA"
                                          "PDF"
                                          "Paren"
                                          "Spacemacs"
                                          "TLS"
                                          "TODO"
                                          "TODOS"
                                          "UI"
                                          "ag"
                                          "alist"
                                          "anzu"
                                          "asm"
                                          "autoload"
                                          "backend"
                                          "cleanroom"
                                          "config"
                                          "contrib"
                                          "descbinds"
                                          "dev"
                                          "doct"
                                          "docview"
                                          "el"
                                          "elfeed"
                                          "emacs"
                                          "emacs-dev"
                                          "emacs.d"
                                          "epg"
                                          "epub"
                                          "esup"
                                          "eval"
                                          "evilified"
                                          "explorg"
                                          "flx"
                                          "flycheck"
                                          "flyspell"
                                          "fontify"
                                          "gc"
                                          "gpg"
                                          "hideshow"
                                          "htmlize"
                                          "init"
                                          "js"
                                          "linter"
                                          "macrostep"
                                          "magit"
                                          "modeline"
                                          "nov"
                                          "olivetti"
                                          "org-plus-contrib"
                                          "package-lint"
                                          "paren"
                                          "pdf"
                                          "ql"
                                          "quickhelp"
                                          "rebase"
                                          "recentf"
                                          "shr"
                                          "smtpmail"
                                          "stardict"
                                          "symlinked"
                                          "toc"
                                          "tongle"
                                          "twbs"
                                          "utils"
                                          "vc"
                                          "vterm"
                                          "wikinforg"
                                          "wn"
                                          "wordnet"
                                          "wordnut"
                                          "writegood"
                                          "yasnippet"
                                          "yasnippets"
                                          "zerodark"
                                          ":PROPERTIES:"))

(provide 'init-dev)

;;; init-dev.el ends here
