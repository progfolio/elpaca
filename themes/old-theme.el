(deftheme old)
;;Assignment form: VARIABLE             COLOR      [TTY-COLOR]
(let ((colors '((banana             "#f4e5b6"      "#ffdf87")
                (buff               "#f0db98"      "#ffdf87")
                (darker-gunmetal    "#17171e"      "#1c1c1c")
                (dark-gunmetal      "#202029"      "#000000")
                (dark-gray          "#252530"      "#808080")
                (dim-gray           "#404040"      "#c0c0c0")
                (gray               "#808084"      "#c0c0c0")
                (light-coral        "#ea8881"      "#d75f00")
                (light-smoke        "#15151c"      "#808080")
                (mint               "#40dd66"      "#00ff00")
                (pale-violet-red    "#da6d8a"      "#d75f5f")
                (rubber             "#d24d71"      "#d75f5f")
                (lavender           "#a469c2"      "#875faf")
                (peach              "#ffc09f"      "#ffaf5f")
                (sky-blue           "#7cc1d6"      "#5fafff")
                (alt-blue           "#4da1d6"      "#0087d7")
                (tomato             "#ff6347"      "#d75f00")
                (smoke              "#131319"      "#121212")))

      (faces '(
               ;;tongle
               (tongle-disabled :background ,light-smoke :foreground ,dim-gray)
               (tongle-replacement :background ,dark-gunmetal :foreground ,banana)
               ;;show-paren
               (show-paren-match-expression  :inherit default :foreground ,lavender :weight bold)
               ;; default
               (cursor  :background ,peach)
               (default :background ,darker-gunmetal :foreground ,banana :weight medium)
               (info-string :foreground  ,peach)
               (region  :background ,dark-gray :foreground  ,lavender :weight bold)
               (secondary-selection  :background ,smoke :foreground ,buff :weight bold)
               (hl-line :background ,alt-blue :foreground ,smoke)
               ;;syntax
               (font-lock-function-name-face :bold t :foreground ,sky-blue)
               (font-lock-keyword-face :bold t :foreground ,rubber)
               (font-lock-builtin-face :bold nil :foreground ,gray :family "Source Code Pro")
               (font-lock-comment-face :foreground ,sky-blue :family "Source Code Pro")
               (font-lock-constant-face :foreground ,sky-blue :bold t)
               (font-lock-string-face :foreground ,light-coral :bold nil)
               ;;mu4e
               (mu4e-view-body-face :background ,darker-gunmetal)
               ;; mode-line
               (mode-line :foreground ,mint :background ,darker-gunmetal :box ,sky-blue)
               (mode-line-buffer-id :background ,tomato :foreground ,darker-gunmetal)
               (mode-line-directory :background ,alt-blue :foreground ,darker-gunmetal)
               (mode-line-inactive :foreground ,mint :background ,darker-gunmetal :box ,sky-blue)
               (mode-line-active :foreground ,darker-gunmetal :background ,mint)
               (mode-line-emphasis :foreground ,darker-gunmetal :background ,peach)
               ;; minibuffer
               (minibuffer-prompt :foreground ,mint :bold t)
               ;; company
               (company-echo-common :foreground ,dark-gunmetal :background ,mint)
               (company-preview :background ,dark-gunmetal :foreground ,lavender)
               (company-preview-common :foreground ,darker-gunmetal :foreground ,mint)
               ;;(company-preview-search :foreground ,type :background ,dark-gunmetal)
               (company-scrollbar-bg :background ,smoke)
               (company-scrollbar-fg :foreground ,alt-blue)
               (company-template-field :inherit region)
               (company-tooltip :foreground ,mint :background ,smoke)
               (company-tooltip-annotation :foreground ,sky-blue)
               (company-tooltip-common  :foreground ,banana)
               (company-tooltip-common-selection :foreground ,banana)
               (company-tooltip-mouse :inherit region)
               (company-tooltip-selection :inherit region)
               (tooltip :foreground ,banana :background ,smoke)
               ;; helm
               ;;(helm-action)
               (helm-bookmark-w3m :foreground ,mint)
               (helm-buffer-not-saved :foreground ,peach :background ,darker-gunmetal)
               (helm-buffer-process :foreground ,mint :background ,dark-gunmetal)
               (helm-buffer-saved-out :foreground ,mint :background ,dark-gunmetal)
               (helm-buffer-size :foreground ,mint :background ,dark-gunmetal)
               (helm-candidate-number :foreground ,dark-gunmetal :background ,mint)
               (helm-ff-directory :foreground ,sky-blue :background ,darker-gunmetal :weight bold)
               (helm-ff-executable :foreground ,mint :background ,darker-gunmetal :weight normal)
               (helm-ff-file :foreground ,banana :background ,darker-gunmetal :weight normal)
               (helm-ff-invalid-symlink :foreground ,light-coral :background ,dark-gunmetal :weight thin :strike-through t)
               (helm-ff-prefix :foreground ,mint :background ,dark-gunmetal :weight normal)
               (helm-ff-symlink :foreground ,alt-blue :background ,dark-gunmetal :weight bold)
               (helm-ff-dotted-directory :foreground ,alt-blue :background ,darker-gunmetal :weight bold)
               (helm-ff-dotted-symlink-directory :foreground ,alt-blue :background ,darker-gunmetal :weight bold)
               ;;(helm-grep-cmd-line :foreground ,mint :background ,dark-gunmetal)
               ;;(helm-grep-file :foreground ,mint :background ,dark-gunmetal)
               ;;(helm-grep-finish :foreground ,mint :background ,dark-gunmetal)
               ;;(helm-grep-lineno :foreground ,mint :background ,dark-gunmetal)
               ;;(helm-grep-match :foreground nil :background nil :inherit helm-match)
               ;;(helm-grep-running :foreground ,mint :background ,dark-gunmetal)
               (helm-header :foreground ,mint :background ,dark-gunmetal :underline nil :box nil)
               ;;(helm-moccur-buffer :foreground ,func :background ,bg1)
               (helm-selection :background "#252530"   :foreground  ,lavender :weight bold :underline nil)
               (helm-selection-line :background ,dark-gunmetal :foreground ,mint)
               (helm-separator :foreground ,dark-gunmetal :background ,sky-blue)
               ;;(helm-source-go-package-godoc-description :foreground ,str)
               (helm-source-header :foreground ,darker-gunmetal :background ,sky-blue
                                   :underline nil :weight bold)
               ;;(helm-time-zone-current :foreground ,builtin :background ,bg1)
               ;;(helm-time-zone-home :foreground ,type :background ,bg1)
               ;;(helm-visible-mark :foreground ,bg1 :background ,bg3)
               ;;###org
               ;;(org-agenda-column-dateline  :foreground ,dim-gray :bold f)
               (org-agenda-current-time :foreground ,alt-blue :bold t )
               (org-agenda-done  :foreground ,dim-gray :bold f)
               (org-agenda-date-today  :foreground ,banana :inherit org-agenda-date :bold t)
               (org-block :background ,smoke :foreground ,mint :weight normal :family "Hermit"
                          )
               (org-block-begin-line :background ,light-smoke :foreground ,dim-gray)
               (org-block-end-line :background ,light-smoke :foreground ,dim-gray)
               (org-code :family "Hermit" :bold t :foreground ,peach)
               (org-column :bold t :foreground ,mint :background ,smoke :family "Hermit")
               (org-date  :foreground ,mint :underline t)
               (org-done  :foreground ,mint :weight bold)
               (org-ellipsis :foreground ,dim-gray)
               (org-habit-alert-face :background ,buff :foreground ,darker-gunmetal :weight bold :box (:style released-button :line-width -3))
               (org-habit-alert-future-face :background "#ffbc64" :inherit org-habit-alert-face)
               (org-habit-overdue-face :background "#BC3232" :inherit org-habit-alert-face)
               (org-habit-ready-face :background "#32BC32" :inherit org-habit-alert-face)
               (org-habit-clear-face :background "#1540aa" :inherit org-habit-alert-face)
               (org-habit-clear-future-face :background "#102099" :inherit org-habit-alert-face)
               (org-headline-done  :foreground ,mint :weight bold)
               (org-level-1 :family "Source Code Pro" :bold t :underline nil :foreground ,sky-blue)
               (org-level-2 :family "Source Code Pro" :bold t :underline nil :foreground ,pale-violet-red)
               (org-link  :foreground ,alt-blue :underline t)
               (org-priority  :inherit org-done)
               (org-scheduled-previously  :foreground ,rubber :bold f)
               (org-special-keyword :foreground ,dim-gray)
               (org-table :background ,smoke :foreground ,mint :weight normal )
               (org-time-grid :foreground ,dim-gray :bold f)
               (org-todo :background nil :foreground ,light-coral :bold t :italic t :family "Source Code Pro")
               (org-upcoming-deadline  :foreground ,rubber :bold f)
               (org-upcoming-distant-deadline :background ,light-smoke :foreground ,dim-gray)
               (org-verbatim :family "Source Code Pro" :bold t :foreground ,sky-blue)
               (org-warning :foreground ,alt-blue :bold t ))))
  (apply #'custom-theme-set-faces
         'old
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

(provide-theme 'old)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; old-theme.el ends here
