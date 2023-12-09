;;; mine-theme.el --- My personal theme ;-*- eval: (progn (rainbow-mode) (fontify-face-mode) (add-hook 'after-save-hook (lambda () (makunbound 'mine-palette) (+change-theme (intern-soft (replace-regexp-in-string "-theme\.el" "" (buffer-name))))) nil t)); lexical-binding: t; -*-
;;; Commentary:
;;
;;; Code:
(defgroup mine-theme nil
  "Options for mine-theme."
  :group 'mine-themes
  :prefix "mine-")

(deftheme mine "My personal Emacs theme.")

(defvar mine-faces nil "Face specs for mine theme.")

(defvar mine-palette
  ;;                 COLOR          TTY-COLOR
  '((blue            "#6698A7"      "#6098A7")
    (gray            "#A2A2A4"      "#A2A2A4")
    (green           "#34B53F"      "#6EBF5B")
    (lavender        "#744A89"      "#905CAA")
    (orange          "#DD845A"      "#FF9868")
    (purple          "#0F0E16"      "#202020")
    (light-purple    "#29263D"      "#202020")
    (bright-purple   "#49465D"      "#202020")
    (dark-purple     "#0E0D14"      "#101010")
    (red             "#dd5668"      "#d64249")
    (yellow          "#AFA27C"      "#C6B78D"))
  "Color palette for mine them.")

(defvar mine-font-sizes '(1.10 1.15 1.25 1.5) "Font sizes for mine theme.")

(defun mine-apply-faces ()
  "Apply theme faces."
  (apply #'custom-theme-set-faces
         'mine
         (let ((color-names (mapcar #'car mine-palette))
               (graphic-mine-palette (mapcar #'cadr mine-palette))
               (tty-mine-palette (mapcar #'car (mapcar #'last mine-palette))))
           (cl-flet* ((expand-for-tty (spec) (cl-progv color-names tty-mine-palette
                                               (eval `(backquote ,spec))))
                      (expand-for-graphic (spec) (cl-progv color-names graphic-mine-palette
                                                   (eval `(backquote ,spec)))))
             (cl-loop for (face . spec) in mine-faces
                      collect `(,face
                                ((((type tty))
                                  ,(expand-for-tty spec))
                                 (((type graphic))
                                  ,(expand-for-graphic spec)))))))))

(defmacro mine-add-faces (package &rest specs)
  "Add PACKAGE SPECS to `mine-faces'."
  (declare (indent 1))
  `(let ((p ',package))
     (dolist (spec (mapcar (lambda (spec)
                             (let* ((abbreviated (car spec))
                                    (full (cond
                                           ((null p) abbreviated)
                                           ((stringp abbreviated) (intern abbreviated))
                                           (t (intern (format "%s-%s" (symbol-name p)
                                                              (symbol-name (car spec))))))))
                               `(,full ,@(cdr spec))))
                           ',specs))
       (if-let ((found (assoc (car spec) mine-faces)))
           (setf (cdr found) (cdr spec))
         (setq mine-faces (nreverse (push spec mine-faces)))))))

(mine-add-faces nil
  (cursor  :background ,green :foreground ,dark-purple :weight bold)
  (default :background ,purple :foreground ,yellow :weight normal :height 130 :distant-foreground ,dark-purple)
  (error :foreground ,red :weight bold)
  (warning :foreground "yellow" :weight bold)
  (fringe :inherit default)
  (header-line :inherit default :underline t)
  (highlight :inherit org-block)
  (isearch :inherit hl-line :box t)
  (lazy-highlight :inherit hl-line :box t)
  (query-replace :background ,yellow :foreground ,dark-purple :box t :weight bold)
  (region  :background ,dark-purple :foreground ,lavender :weight bold :extend t)
  (secondary-selection  :background ,dark-purple :foreground ,yellow :weight bold :extend t)
  (tooltip :foreground ,yellow :background ,dark-purple))

(mine-add-faces company
  (echo-common :foreground ,dark-purple :background ,green :weight bold)
  (preview :background ,dark-purple :foreground ,lavender :weight bold)
  (preview-common :foreground ,purple :foreground ,green)
  (preview-search :inherit highlight)
  (scrollbar-bg :background "#0B0B11")
  (scrollbar-fg :background ,blue)
  (template-field :inherit region)
  (tooltip :inherit default :foreground ,green :background ,purple)
  (tooltip-annotation :foreground ,blue)
  (tooltip-common  :foreground ,yellow)
  (tooltip-common-selection :foreground ,yellow)
  (tooltip-mouse :inherit region)
  (tooltip-selection :extend t :weight bold :background "#0B0B11"))

(mine-add-faces diff
  (header :background ,dark-purple)
  (file-header :background ,light-purple)
  (added :background ,dark-purple :foreground ,green)
  (refine-added :background ,light-purple :foreground ,green)
  (refine-removed :background ,light-purple :foreground ,red)
  (indicator-added :background ,dark-purple :foreground ,green)
  (removed :background ,dark-purple :foreground ,red))

(mine-add-faces doom-modeline
  (bar :inherit default)
  (buffer-file :background unspecified :foreground ,red :weight bold)
  (buffer-file :foreground ,blue :weight bold)
  (buffer-major-mode :foreground ,blue :weight ultra-bold :box t)
  (buffer-minor-mode :inherit doom-modeline-buffer-major-mode :weight normal)
  (buffer-modified :foreground ,red :background unspecified :weight bold)
  (buffer-path :background unspecified :foreground ,green :weight bold)
  (evil-insert-state :foreground ,red :weight ultra-bold)
  (evil-normal-state :foreground ,green :weight ultra-bold)
  (evil-replace-state :foreground ,red)
  (evil-visual-state :foreground ,lavender :weight bold)
  (highlight :inherit region :box ,blue)
  (inactive-bar :background unspecified :foreground ,gray)
  (info :foreground ,red :weight bold)
  (panel :inherit default)
  (project-dir :foreground ,gray :background unspecified :weight ultra-bold)
  (project-dir :inherit default)
  (project-parent-dir :foreground ,gray :weight bold)
  (project-root-dir :inherit default)
  (unread-number :inherit default :box t :weight bold)
  (warning :foreground ,red :box ,blue))

(mine-add-faces elfeed
  (search-date-face  :foreground ,blue)
  (search-feed-face  :foreground ,blue)
  (search-filter-face :box ,yellow  :foreground ,red)
  (search-tag-face   :foreground ,green)
  (search-title-face :inherit font-lock-builtin-face)
  (search-unread-count-face :foreground ,green :box ,yellow)
  (search-unread-title-face :inherit default :height ,(nth 0 mine-font-sizes) :weight bold))

(mine-add-faces ement
  (room-quote :foreground ,bright-purple :background ,dark-purple :extend t :height unspecified)
  (room-self :foreground ,green :weight ultra-bold)
  (room-self-message :foreground ,green))

(mine-add-faces erc
  (input-face :foreground ,red)
  (my-nick-face :foreground ,green)
  (nick-msg-face :foreground ,red :weight bold))

(mine-add-faces ert
  (test-result-expected   :background ,dark-purple :foreground ,green)
  (test-result-unexpected :background ,dark-purple :foreground ,red :box ,red))

(mine-add-faces evil
  (ex-substitute-replacement :background ,green :foreground ,purple :weight bold :slant italic :box t))

(mine-add-faces fill-column
  (indicator :foreground ,red))

(mine-add-faces font-lock
  (builtin-face :weight normal :foreground ,gray)
  (comment-face :foreground ,blue)
  (constant-face :foreground ,blue :weight bold)
  (function-name-face :weight bold :foreground ,blue)
  (keyword-face :weight bold :foreground ,red)
  (string-face :foreground ,red :weight normal)
  (type-face :foreground ,green)
  (variable-name-face :foreground ,blue :weight normal)
  (warning-face :foreground ,red :weight bold))

(mine-add-faces helm
  (M-x-key :foreground ,blue :underline t :weight bold)
  (buffer-directory :background ,lavender :foreground ,dark-purple)
  (buffer-not-saved :foreground ,red)
  (buffer-process :foreground ,green)
  (buffer-size :foreground ,green)
  (candidate-number :background ,green :foreground ,dark-purple)
  (ff-directory :foreground ,blue :weight bold)
  (ff-dotted-directory :inherit helm-ff-directory)
  (ff-dotted-symlink-directory :inherit helm-ff-symlink)
  (ff-executable :foreground ,green :weight normal)
  (ff-file :inherit default)
  (ff-file-extension :foreground ,green :weight normal)
  (ff-invalid-symlink :foreground ,red :weight light :strike-through t)
  (ff-prefix :foreground ,green :weight normal)
  (ff-symlink :foreground ,blue :italic t :weight bold)
  (header :foreground ,green :underline nil :box nil)
  (lisp-show-completion :inherit org-code)
  (match :foreground ,green :weight bold :box t)
  (minibuffer-prompt :foreground ,green :weight bold :background ,purple :extend t)
  (selection :extend t :weight bold :background "#0B0B11")
  (selection-line :foreground ,red :background ,purple :box ,red :extend t)
  (separator :foreground ,blue :extend t)
  (source-header :background ,yellow :extend t :foreground ,dark-purple :height ,(nth 2 mine-font-sizes) :weight bold))

(mine-add-faces help
  (key-binding :weight bold :inherit defualt :foreground ,green))

(mine-add-faces hl
  (line :background ,light-purple :weight bold :extend t))

(mine-add-faces info
  (string :foreground ,green))

(mine-add-faces magit
  (section-highlight :background ,light-purple)
  (hash :foreground ,gray))

(mine-add-faces message
  (header-name :inherit org-block)
  (header-other :inherit default)
  (header-subject :inherit default :foreground ,red :weight bold :height ,(nth 1 mine-font-sizes))
  (header-to :inherit font-lock-comment-face :height ,(nth 0 mine-font-sizes)))

(mine-add-faces mode-line
  ("mode-line" :foreground ,green :background ,purple :box ,blue)
  (active :inherit mode-line :box ,green)
  (buffer-id :background ,red :foreground ,purple :weight bold)
  (directory :background ,blue :foreground ,purple :weight bold)
  (emphasis :foreground ,purple :background ,green)
  (inactive :foreground ,gray :box ,gray))

(mine-add-faces mu4e
  (contact-face :inherit font-lock-comment-face)
  (header-value-face :inherit font-lock-string-face :weight bold)
  (link-face :inherit default :underline t)
  (title-face :inherit font-lock-comment-face :weight ultra-bold)
  (url-number-face :inherit font-lock-comment-face :weight bold)
  (view-body-face :background ,purple))

(mine-add-faces minibuffer
  (prompt :foreground ,green :weight bold))

(mine-add-faces org
  (agenda-current-time :foreground ,blue :weight bold)
  (agenda-date-today :foreground ,yellow :inherit org-agenda-date :weight bold)
  (agenda-dimmed-todo-face :foreground ,gray :weight bold)
  (agenda-done :foreground ,gray :weight bold)
  (block :background ,dark-purple :foreground ,green :weight normal  :extend t)
  (block-begin-line :foreground ,gray :extend t)
  (block-end-line :background unspecified :foreground ,gray :extend t)
  (checkbox-statistics-todo :foreground ,green :weight ultra-bold :slant italic)
  (code  :inherit org-block)
  (column :weight bold :foreground ,green :background ,dark-purple )
  (date  :foreground ,green :underline t)
  (document-info-keyword :foreground ,gray)
  (document-title :inherit default :weight bold :height ,(nth 2 mine-font-sizes))
  (done  :foreground ,green :weight bold)
  (ellipsis :foreground ,gray :weight normal :weight semi-light)
  (habit-alert-face :background ,yellow :foreground ,purple :weight ultra-bold :height 1.5 :box (:style pressed-button :line-width 1 :color ,purple))
  (habit-alert-future-face :background ,yellow :inherit org-habit-alert-face)
  (habit-clear-face :background ,blue :inherit org-habit-alert-face)
  (habit-clear-future-face :background ,blue :inherit org-habit-alert-face)
  (habit-overdue-face :background ,red :inherit org-habit-alert-face)
  (habit-ready-face :background ,green :inherit org-habit-alert-face)
  (headline-done  :foreground ,green :weight bold)
  (level-1 :foreground ,orange :weight bold :height ,(nth 0 mine-font-sizes))
  (level-2 :foreground ,blue :weight bold)
  (level-3 :foreground ,lavender :weight bold)
  (level-4 :foreground ,yellow :weight bold)
  (level-5 :foreground ,orange :weight bold)
  (level-6 :foreground ,blue :weight bold)
  (level-7 :foreground ,lavender :weight bold)
  (level-8 :foreground ,yellow :weight bold)
  (link  :underline t)
  (meta-line :foreground ,gray)
  (priority  :inherit org-done)
  (scheduled :foreground ,blue :weight bold)
  (scheduled-previously  :foreground ,red :weight bold)
  (scheduled-today  :foreground ,green :weight bold)
  (special-keyword :foreground ,gray)
  (table :background ,dark-purple :foreground ,green :weight normal)
  (tag :foreground ,blue :height 1.0)
  (time-grid :foreground ,gray)
  (todo :foreground ,red :weight bold :slant italic :height ,(nth 0 mine-font-sizes))
  (upcoming-deadline  :foreground ,red :weight bold)
  (upcoming-distant-deadline :foreground ,gray)
  (verbatim  :weight bold :foreground ,blue)
  (warning :foreground ,blue :weight bold))

(mine-add-faces show-paren
  (match-expression  :inherit default :foreground ,lavender :weight bold))

(mine-add-faces tab-bar
  ("tab-bar" :inherit default)
  (tab :inherit default :height 0.9 :foreground ,red :box (:line-width -3 :color ,dark-purple :style released-button))
  (tab-inactive :inherit default :height 0.9 :background ,dark-purple :foreground ,gray))

(mine-add-faces tab-line
  ("tab-line" :inherit tab-bar)
  (tab :inhert tab-bar-tab)
  (tab-current  :inherit tab-bar-tab :foreground ,red)
  (tab-inactive :inherit tab-bar-tab-inactive))

(mine-add-faces tongle
  (disabled :foreground ,gray :extend t)
  (replacement :background ,gray :foreground ,yellow :extend t))

(mine-add-faces vterm
  (color-black       :background ,gray)
  (color-blue        :foreground "#2000a7")
  (color-cyan        :foreground ,blue)
  (color-dark-purple :foreground ,purple)
  (color-default     :inherit default)
  (color-green       :foreground ,green)
  (color-magenta     :foreground ,blue)
  (color-red         :foreground ,red)
  (color-white       :foreground ,gray)
  (color-yellow      :foreground ,yellow))

(mine-add-faces widget
  (field :background ,dark-purple :foreground ,green)
  (single-line-field :background ,dark-purple :foreground ,green))

(mine-add-faces reb
  (match-0 :background ,red :foreground ,dark-purple :weight bold)
  (match-1 :background ,blue :foreground ,dark-purple :weight bold)
  (match-2 :background ,yellow :foreground ,dark-purple :weight bold)
  (match-3 :background ,orange :foreground ,dark-purple :weight bold))

(mine-add-faces shr
  (link :inherit org-link)
  (selected-link)
  (strike-through))

(mine-add-faces smerge
  (lower :background ,green :foreground , purple)
  (refined-added :background ,green :foreground ,dark-purple :weight ultra-bold))

(mine-add-faces wikinfo
  (search-title :foreground ,blue :weight ultra-bold :underline t))

(mine-apply-faces)
;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))
(provide-theme 'mine)
;; Local Variables:
;; no-byte-compile: t
;; End:
;;; mine-theme.el ends here
