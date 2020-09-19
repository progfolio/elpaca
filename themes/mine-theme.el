;-*- eval: (progn (rainbow-mode) (fontify-face-mode) (add-hook 'after-save-hook (lambda () (my/change-theme (intern-soft (replace-regexp-in-string "-theme\.el" "" (buffer-name))))) nil t));   -*-
;;; mine-theme.el --- My personal theme

;;; Commentary:
;;

;;; Code:
(deftheme mine)

(let* ((font-sizes '(1.10 1.15 1.25 1.5))
       ;;                      COLOR       TTY-COLOR
       (colors '((dark-purple "#0E0D14"      "#101010")
                 (gray        "#525254"      "#525254")
                 (green       "#34B53F"      "#6EBF5B")
                 (lavender    "#744A89"      "#905CAA")
                 (orange      "#DD845A"      "#FF9868")
                 (purple      "#0F0E16"      "#202020")
                 (red         "#dd5668"      "#d64249")
                 (blue        "#6698A7"      "#6098A7")
                 (yellow      "#AFA27C"      "#C6B78D")))

       (faces '(;; basics
                (cursor  :background ,green :foreground ,dark-purple :weight bold)
                (default :background ,purple :foreground ,yellow :weight normal :height 97)
                ;;not sure about this yet
                (fringe :inherit default)
                (header-line :inherit default :underline t)
                (highlight :inherit org-block)
                (hl-line :background ,red :foreground ,dark-purple :weight bold :extend t)
                (info-string :foreground  ,green)
                (region  :background ,dark-purple :foreground ,lavender :weight bold :extend t)
                (secondary-selection  :background ,dark-purple :foreground ,yellow :weight bold :extend t)
                ;;widgets
                (widget-field :background ,dark-purple :foreground ,green)
                (widget-single-line-field :background ,dark-purple :foreground ,green)
                ;;fill-column
                (fill-column-indicator :foreground ,red)
                ;;vterm
                (vterm-color-default  :inherit default :weight bold)
                (vterm-color-dark-purple    :foreground ,purple)
                (vterm-color-blue     :foreground ,blue)
                (vterm-color-cyan     :foreground ,blue)
                (vterm-color-green    :foreground ,green)
                (vterm-color-magenta  :foreground ,blue)
                (vterm-color-red      :foreground ,red)
                (vterm-color-white    :foreground ,gray)
                (vterm-color-yellow   :foreground ,yellow)
                ;;tab-bar
                (tab-bar :inherit default)
                (tab-bar-tab :inherit default :height 0.9 :foreground ,red :box (:line-width -3 :color ,dark-purple :style released-button))
                (tab-bar-tab-inactive :inherit default :height 0.9 :background ,dark-purple :foreground ,gray)
                ;;tab-line
                (tab-line :inherit tab-bar)
                (tab-line-tab :inhert tab-bar-tab)
                (tab-line-tab-inactive :inherit tab-bar-tab-inactive)
                (tab-line-tab-current  :inherit tab-bar-tab :foreground ,red)
                ;;tongle
                (tongle-disabled :foreground ,gray :extend t)
                (tongle-replacement :background ,gray :foreground ,yellow :extend t)
                ;;show-paren
                (show-paren-match-expression  :inherit default :foreground ,lavender :weight bold)
                ;;elfeed
                (elfeed-search-tag-face   :foreground ,green)
                (elfeed-search-feed-face  :foreground ,blue)
                (elfeed-search-date-face  :foreground ,blue)
                (elfeed-search-unread-title-face :inherit default :height ,(nth 0 font-sizes) :weight bold)
                (elfeed-search-unread-count-face :foreground ,green :box ,yellow)
                (elfeed-search-filter-face :box ,yellow  :foreground ,red)
                (elfeed-search-title-face :inherit font-lock-builtin-face)
                ;;message
                (message-header-name :inherit org-block)
                (message-header-subject :inherit default :foreground ,red :weight bold :height ,(nth 1 font-sizes))
                (message-header-to :inherit font-lock-comment-face :height ,(nth 0 font-sizes))
                (message-header-other :inherit default)
                ;;syntax
                (font-lock-function-name-face :bold t :foreground ,blue)
                (font-lock-type-face :foreground ,green)
                (font-lock-keyword-face :bold t :foreground ,red)
                (font-lock-builtin-face :bold nil :foreground ,gray)
                (font-lock-comment-face :foreground ,blue)
                (font-lock-constant-face :foreground ,blue :bold t)
                (font-lock-string-face :foreground ,red :bold nil)
                (font-lock-variable-name-face :foreground ,blue :bold nil)
                (font-lock-warning-face :foreground ,red :bold t)
                ;;search/replace
                (lazy-highlight :inherit hl-line :box t)
                (query-replace :background ,yellow :foreground ,dark-purple :box t :weight bold)
                (isearch :inherit hl-line :box t)
                (evil-ex-substitute-replacement :background ,green :foreground ,purple :weight bold :slant italic :box t)
                ;;mu4e
                (mu4e-title-face :inherit font-lock-comment-face :weight ultra-bold)
                (mu4e-view-body-face :background ,purple)
                (mu4e-contact-face :inherit font-lock-comment-face)
                (mu4e-header-value-face :inherit font-lock-string-face :weight bold)
                (mu4e-link-face :inherit default :underline t)
                (mu4e-url-number-face :inherit font-lock-comment-face :weight bold)
                ;; mode-line
                (mode-line :foreground ,green :background ,purple :box ,blue)
                (mode-line-buffer-id :background ,red :foreground ,purple :weight bold)
                (mode-line-directory :background ,blue :foreground ,purple :weight bold)
                (mode-line-inactive :foreground ,gray :box ,blue)
                (mode-line-active :foreground ,purple :background ,green)
                (mode-line-emphasis :foreground ,purple :background ,green)

                ;;doom-modeline
                (doom-modeline-bar :inherit default)
                (doom-modeline-buffer-file :foreground ,blue :weight bold)
                (doom-modeline-buffer-major-mode :foreground ,blue :weight ultra-bold :box t)
                (doom-modeline-buffer-minor-mode :inherit doom-modeline-buffer-major-mode :weight normal)
                (doom-modeline-buffer-modified :foreground ,red :weight bold)
                (doom-modeline-buffer-path :background nil :foreground ,green :weight bold)
                (doom-modeline-buffer-file :background nil :foreground ,red :weight bold)
                ;;(doom-modeline-debug)
                ;;(doom-modeline-evil-emacs-state)
                (doom-modeline-evil-insert-state :foreground ,red :weight ultra-bold)
                ;;(doom-modeline-evil-motion-state)
                (doom-modeline-evil-normal-state :foreground ,green :weight ultra-bold)
                ;;(doom-modeline-evil-operator-state)
                (doom-modeline-evil-replace-state :foreground ,red)
                (doom-modeline-evil-visual-state :foreground ,lavender :weight bold)
                (doom-modeline-highlight :inherit region :box ,blue)
                (doom-modeline-inactive-bar :background nil :foreground ,gray)
                (doom-modeline-info :foreground ,red :weight bold)
                ;;(doom-modeline-lsp-error)
                ;;(doom-modeline-lsp-success)
                ;;(doom-modeline-lsp-warning)
                (doom-modeline-panel :inherit default)
                (doom-modeline-project-dir :foreground ,gray :background nil :weight ultra-bold)
                ;;(doom-modeline-persp-name)
                (doom-modeline-project-dir :inherit default)
                (doom-modeline-project-parent-dir :foreground ,gray :weight bold)
                (doom-modeline-project-root-dir :inherit default)
                (doom-modeline-unread-number :inherit default :box t :weight bold)
                ;;(doom-modeline-urgent)
                (doom-modeline-warning :foreground ,red :box ,blue)


                ;;all-the-icons

                ;; minibuffer
                (minibuffer-prompt :foreground ,green :bold t)
                ;; company
                (company-echo-common :foreground ,dark-purple :background ,green :weight bold)
                (company-preview :background ,dark-purple :foreground ,lavender :weight bold)
                (company-preview-common :foreground ,purple :foreground ,green)
                (company-preview-search :inherit highlight)
                (company-scrollbar-bg :background "#0B0B11")
                (company-scrollbar-fg :background ,blue)
                (company-template-field :inherit region)
                (company-tooltip :inherit default :foreground ,green :background ,purple)
                (company-tooltip-annotation :foreground ,blue)
                (company-tooltip-common  :foreground ,yellow)
                (company-tooltip-common-selection :foreground ,yellow)
                (company-tooltip-mouse :inherit region)
                (company-tooltip-selection :extend t :weight bold :background "#0B0B11")
                (tooltip :foreground ,yellow :background ,dark-purple)
                ;; helm
                ;;(helm-action)
                ;;(helm-bookmark-w3m :foreground ,green)
                (helm-buffer-not-saved :foreground ,red)
                (helm-buffer-directory :background ,lavender :foreground ,dark-purple)
                (helm-buffer-process :foreground ,green)
                ;;helm-buffer-saved-out :foreground ,green :background ,gray)
                (helm-buffer-size :foreground ,green)
                (helm-candidate-number :background ,green :foreground ,dark-purple)
                (helm-ff-directory :foreground ,blue :weight bold)
                (helm-ff-executable :foreground ,green :weight normal)
                (helm-ff-file :inherit default)
                (helm-ff-file-extension :foreground ,red :weight normal)
                (helm-ff-invalid-symlink :foreground ,red :weight thin :strike-through t)
                (helm-ff-prefix :foreground ,green :weight normal)
                (helm-ff-symlink :foreground ,blue :italic t :weight bold)
                (helm-ff-dotted-directory :inherit helm-ff-directory)
                (helm-ff-dotted-symlink-directory :inherit helm-ff-symlink)
                ;;(helm-grep-cmd-line :foreground ,green :background ,gray)
                ;;(helm-grep-file :foreground ,green :background ,gray)
                ;;(helm-grep-finish :foreground ,green :background ,gray)
                ;;(helm-grep-lineno :foreground ,green :background ,gray)
                ;;(helm-grep-match :foreground nil :background nil :inherit helm-match)
                ;;(helm-grep-running :foreground ,green :background ,gray)

                (helm-header :foreground ,green :underline nil :box nil)
                ;;(helm-moccur-buffer :foreground ,func :background ,bg1)
                (helm-M-x-key :foreground ,blue :underline t :weight bold)
                (helm-match :foreground ,green :weight bold :box t)
                (helm-minibuffer-prompt :foreground ,green :weight bold :background ,purple :extend t)
                (helm-selection :extend t :weight bold :background "#0B0B11")
                (helm-selection-line :foreground ,red :background ,purple :box ,red :extend t)
                (helm-separator :foreground ,blue :extend t)
                ;;(helm-source-go-package-godoc-description :foreground ,str)
                (helm-source-header
                 :height ,(nth 2 font-sizes)
                 :weight bold
                 :foreground ,dark-purple
                 :background ,yellow
                 :extend t)
                ;;(helm-time-zone-current :foreground ,builtin :background ,bg1)
                ;;(helm-time-zone-home :foreground ,type :background ,bg1)
                ;;(helm-visible-mark :foreground ,bg1 :background ,bg3)

                ;;###org
                ;;(org-agenda-column-dateline  :foreground ,gray :bold f)
                (org-agenda-dimmed-todo-face :foreground ,gray :bold t)
                (org-agenda-current-time :foreground ,blue :bold t)
                (org-agenda-done  :foreground ,gray :bold f)
                (org-agenda-date-today  :foreground ,yellow :inherit org-agenda-date :bold t)
                (org-block :background ,dark-purple :foreground ,green :weight normal  :extend t)
                (org-block-begin-line :foreground ,gray :extend t)
                (org-block-end-line :background nil :foreground ,gray :extend t)
                (org-code  :inherit org-block)
                (org-column :bold t :foreground ,green :background ,dark-purple )
                (org-date  :foreground ,green :underline t)
                (org-done  :foreground ,green :weight bold)
                (org-ellipsis :foreground ,gray :weight normal :weight semi-light)
                (org-habit-alert-face :background ,yellow :foreground ,purple :weight ultra-bold :height 1.5
                                      :box (:style pressed-button :line-width 1 :color ,purple))
                (org-habit-alert-future-face :background ,yellow :inherit org-habit-alert-face)
                (org-habit-overdue-face :background ,red :inherit org-habit-alert-face)
                (org-habit-ready-face :background ,green :inherit org-habit-alert-face)
                (org-habit-clear-face :background ,blue :inherit org-habit-alert-face)
                (org-habit-clear-future-face :background ,blue :inherit org-habit-alert-face)
                (org-headline-done  :foreground ,green :weight bold)

                (org-level-1 :foreground ,orange :weight bold :height ,(nth 0 font-sizes))
                (org-level-2 :foreground ,blue :weight bold)
                (org-level-3 :foreground ,lavender :weight bold)
                (org-level-4 :foreground ,yellow :weight bold)
                (org-level-5 :foreground ,orange :weight bold)
                (org-level-6 :foreground ,blue :weight bold)
                (org-level-7 :foreground ,lavender :weight bold)
                (org-level-8 :foreground ,yellow :weight bold)

                (org-link  :underline t)
                (org-priority  :inherit org-done)
                (org-scheduled-previously  :foreground ,red :bold f)
                (org-scheduled :foreground ,blue :bold f)
                (org-scheduled-today  :foreground ,green :bold f)
                (org-special-keyword :foreground ,gray)
                (org-table :background ,dark-purple :foreground ,green :weight normal )
                (org-time-grid :foreground ,gray)
                (org-document-info-keyword :foreground ,gray)
                (org-document-title :inherit default :weight bold :height ,(nth 2 font-sizes))
                (org-meta-line :foreground ,gray)
                (org-tag :foreground ,blue :height 1.0)
                (org-todo :foreground ,red :weight bold :slant italic :height ,(nth 0 font-sizes))
                (org-checkbox-statistics-todo :foreground ,green :weight ultra-bold :slant italic)
                (org-upcoming-deadline  :foreground ,red :bold f)
                (org-upcoming-distant-deadline :foreground ,gray)
                (org-verbatim  :bold t :foreground ,blue)
                (org-warning :foreground ,blue :bold t )
                ;;reb
                (reb-match-0 :background ,red :foreground ,dark-purple :weight bold)
                (reb-match-1 :background ,blue :foreground ,dark-purple :weight bold)
                (reb-match-2 :background ,yellow :foreground ,dark-purple :weight bold)
                (reb-match-3 :background ,orange :foreground ,dark-purple :weight bold)
                ;;shr
                ;;shr-abbreviation
                (shr-link :inherit org-link)
                (shr-selected-link)
                (shr-strike-through)

                ;;ert
                (ert-test-result-expected   :background ,dark-purple :foreground ,green)
                (ert-test-result-unexpected :background ,dark-purple :foreground ,red :box ,red)
                ;;wikinfo
                (wikinfo-search-title :foreground ,blue :weight ultra-bold :underline t)

                )))
  (apply #'custom-theme-set-faces
         'mine
         (let ((color-names (mapcar #'car colors))
               (graphic-colors (mapcar #'cadr colors))
               (tty-colors (mapcar #'car (mapcar #'last colors))))
           (cl-flet* ((expand-for-tty (spec) (cl-progv color-names tty-colors
                                               (eval `(backquote ,spec))))
                      (expand-for-graphic (spec) (cl-progv color-names graphic-colors
                                                   (eval `(backquote ,spec)))))
             (cl-loop for (face . spec) in faces
                      collect `(,face
                                ((((type tty))
                                  ,(expand-for-tty spec))
                                 (((type graphic))
                                  ,(expand-for-graphic spec)))))))))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'mine)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; mine-theme.el ends here
