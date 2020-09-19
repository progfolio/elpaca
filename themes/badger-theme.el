;;; badger-theme.el --- A dark theme for Emacs 24.  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Cody Canning

;; Author: Cody Canning <cocanning11@gmail.com>
;; URL: https://github.com/ccann/badger-theme
;; Version: 1.0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Credits:

;; The structure of this theme was based on zenburn-theme.el by Bozhidar Batsov

;;; Code:
(deftheme badger "The Badger color theme")

;; Badger Color Pallette
(defvar badger-colors-alist
  '(("badger-fg"            . "#F6F3E8")
    ("badger-fg+1"          . "#FBF9F3")
    ("badger-bg"            . "#171717")
    ("badger-bg+1"          . "#2F2F2F")

;; Primary Hues
    ("badger-blue"          . "#8AC6F2")
    ("badger-charcoal"      . "#656868")
    ("badger-salmon"        . "#F28B86")
    ("badger-violet"        . "#BF93C3")
    ("badger-orange"        . "#EA9847")
    ("badger-green"         . "#86B187")
    ("badger-yellow"        . "#E0D063")
    ("badger-sand"          . "#C7B299")

;; Secondary Hues
    ("badger-lime"          . "#84C452")
    ("badger-teal"          . "#65A399")
    ("badger-pink"          . "#E18CBB")
    ("badger-brown"         . "#AC8952")
    ("badger-red"           . "#E2434C")
    ("badger-dull-red"      . "#A55662")
    ("badger-dark-violet"   . "#635770")
    ("badger-darker-violet" . "#433F4F")
    ("badger-olive"         . "#9AA68E")

;; Misc.
    ("badger-link"          . "#8ACDAA")
    ("badger-warn"          . "magenta")
    ("badger-succ"          . "cyan")
    ("badger-hl"            . "#1D1D1D")))

(defmacro badger/with-color-variables (&rest body)
  "`let' bind all colors defined in `badger-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   badger-colors-alist))
     ,@body))

(badger/with-color-variables 
  (custom-theme-set-faces
   'badger

   ;; >>>>> Built-in
   '(button ((t (:underline t))))
   `(link ((t (:bold t :foreground ,badger-blue :underline t :weight bold))))
   ;; `(link-visited ((t (:foreground ,badger-salmon-2 :underline t :weight normal))))

   ;; ordinary text. Its background color is used as the frame's background color. 
   `(default ((t (:foreground ,badger-fg :background ,badger-bg))))

   ;;The :background attribute of this face specifies the color of the text cursor
   `(cursor ((t (:background ,badger-salmon)))) 

   ;; The face for displaying control characters and escape sequences
   `(escape-glyph ((t (:foreground ,badger-salmon :bold t))))

   ;; The face for the narrow fringes to the left and right of windows on graphic displays.
   `(fringe ((t (:foreground ,badger-fg :background ,"black"))))

   ;; fixed line displayed at the top of the emacs window, not in XEmacs
   ;; `(header-line ((t (:foreground ,badger-salmon 
   ;;                                :background ,"black"
   ;;                                :box (:line-width -1 :style released-button)))))

   ;;text highlighting in various contexts, when the mouse cursor is moved over a hyperlink. 
   `(highlight ((t (:background ,badger-hl))))

   ;; “lazy matches” for Isearch and Query Replace (matches other than the current one). 
   `(lazy-highlight ((t (:background ,badger-yellow :foreground ,"black" :weight extra-bold))))

   ;; This face is used to highlight the current Isearch match 
   `(isearch ((t (:background ,badger-succ :foreground ,"black" :weight extra-bold))))

   
   `(success ((t (:foreground ,badger-link :weight bold))))
   `(warning ((t (:foreground ,badger-pink :weight bold)))) 

   ;; This face is used for displaying an active region 
   `(region ((t (:background ,"black"))))

   `(show-paren-match-face ((t (:background ,badger-lime :foreground ,"black" ))))

   ;; >>>>> mode-line
   `(mode-line    ((,class (:foreground ,badger-charcoal
                                        :background ,"black"
                                       ;; :box (:line-width -1 :style released-button)
                                        ))
                   (t :inverse-video nil)))

   `(mode-line-inactive ((t (:background ,badger-bg+1 :foreground ,"black" :box nil))))
   `(mode-line-buffer-id ((t (:foreground ,badger-salmon))))
   `(minibuffer-prompt ((t (:foreground ,badger-violet))))

   ;;   `(mode-line-highlight ((t (:foreground ,badger-lime))))

   ;; linum
   `(linum ((t (:foreground ,badger-charcoal :background ,"black"))))
   

   ;; >>>>> font-lock
   `(font-lock-warning-face ((t (:foreground ,badger-yellow :weight bold))))
   `(font-lock-function-name-face ((t (:foreground ,badger-orange ))))
   `(font-lock-variable-name-face ((t (:foreground ,badger-salmon))))
   `(font-lock-keyword-face ((t (:foreground ,badger-blue))))
   `(font-lock-comment-face ((t (:foreground ,badger-charcoal))))
   ;;`(font-lock-comment-delimiter-face ((t (:foreground ,badger-charcoal :weight light :slant italic))))
   `(font-lock-type-face ((t (:foreground ,badger-sand))))
   `(font-lock-constant-face ((t (:foreground ,badger-dark-violet))))
   `(font-lock-builtin-face ((t (:foreground ,badger-violet))))
   `(font-lock-preprocessor-face ((t (:foreground ,badger-sand))))
   `(font-lock-string-face ((t (:foreground ,badger-green))))
 ;;  `(font-lock-doc-face ((t (:foreground ,badger-green))))
   

   ;; >>>>> eshell 
   `(eshell-prompt ((t (:foreground ,badger-lime))))
   `(eshell-ls-archive ((t (:foreground ,badger-orange :weight bold))))
   `(eshell-ls-backup ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-clutter ((t (:inherit font-lock-comment-face))))
   `(eshell-ls-directory ((t (:foreground ,badger-violet :weight normal))))
   `(eshell-ls-executable ((t (:foreground ,badger-yellow :weight normal))))
   `(eshell-ls-unreadable ((t (:foreground ,badger-fg))))
   `(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
   `(eshell-ls-product ((t (:inherit font-lock-doc-face))))
   `(eshell-ls-special ((t (:foreground ,badger-blue :weight bold))))
   `(eshell-ls-symlink ((t (:foreground ,badger-link :weight bold))))

   ;; >>>>> Org mode
   `(org-document-info-keyword ((t (:foreground ,badger-olive))))
   `(org-document-title ((t (:foreground ,badger-salmon :height 1.50))))
   `(org-archived ((t (:foreground ,badger-fg :weight bold))))
   `(org-checkbox ((t (:foreground ,badger-fg+1 :foreground ,badger-olive 
                                   :box (:line-width 1 :style released-button)))))
   `(org-done ((t (:foreground ,badger-lime :strike-through t))))
   `(org-todo ((t (:foreground ,badger-red))))
   `(org-formula ((t (:foreground ,badger-violet))))
   `(org-headline-done ((t (:strike-through t :foreground ,badger-charcoal))))
   `(org-hide ((t (:foreground ,badger-bg)))) 
   `(org-level-1 ((t (:foreground ,badger-blue))))
   `(org-level-2 ((t (:foreground ,badger-violet))))
   `(org-level-3 ((t (:foreground ,badger-orange))))
   `(org-level-4 ((t (:foreground ,badger-yellow))))
   `(org-level-5 ((t (:foreground ,badger-salmon))))
   `(org-level-6 ((t (:foreground ,badger-green))))
   `(org-level-7 ((t (:foreground ,badger-brown))))
   `(org-level-8 ((t (:foreground ,badger-teal))))
   `(org-link ((t (:foreground ,badger-link :underline t))))
   
   `(org-agenda-date ((t (:foreground ,badger-blue))))
   `(org-deadline-announce ((t (:foreground ,badger-dull-red))))
   `(org-date ((t (:foreground ,badger-link :underline t))))
   `(org-agenda-date-today  ((t (:foreground ,badger-salmon :weight light :slant italic))))
   `(org-agenda-structure  ((t (:inherit font-lock-comment-face))))
   ;; `(org-scheduled ((t (:foreground ,zenburn-green+4))))x
   ;; `(org-scheduled-previously ((t (:foreground ,zenburn-red-4))))
   ;; `(org-scheduled-today ((t (:foreground ,zenburn-blue+1))))
   ;; `(org-sexp-date ((t (:foreground ,zenburn-blue+1 :underline t))))
   ;; `(org-time-grid ((t (:foreground ,zenburn-orange))))
   ;; `(org-upcoming-deadline ((t (:inherit font-lock-keyword-face))))

   `(org-special-keyword ((t (:foreground ,badger-olive :weight normal))))
   `(org-table ((t (:foreground ,badger-olive))))
   `(org-tag ((t (:bold t :foreground ,badger-orange :strike-through nil))))
   `(org-warning ((t (:bold t :foreground ,badger-pink :weight bold))))
   `(org-column ((t (:background ,"black"))))
   `(org-column-title ((t (:background ,"black" :foreground ,badger-lime :underline t))))
   `(org-mode-line-clock ((t (:foreground ,badger-yellow))))
   `(org-footnote ((t (:foreground ,badger-link :underline t))))
   `(org-code ((t (:foreground ,badger-olive))))
   `(org-verbatim ((t (:inherit org-code))))
   
   ;; >>>>> elpy and ipython
   `(highlight-indentation-face ((t (:background ,badger-bg))))
   `(comint-highlight-prompt ((t (:inherit eshell-prompt))))
   
   ;; >>>>> auto-complete and popup
   `(ac-candidate-face ((t (:background ,badger-sand :foreground ,"black"))))
   `(ac-selection-face ((t (:background ,badger-violet :foreground ,"black"))))
   `(popup-tip-face ((t (:background ,badger-sand :foreground ,"black"))))
   `(popup-scroll-bar-foreground-face ((t (:background ,badger-dark-violet))))
   `(popup-scroll-bar-background-face ((t (:background ,badger-olive))))
   `(popup-isearch-match ((t (:background ,badger-yellow :foreground ,"black"))))

   ;; >>>>> smart-mode-line
   ;;`(sml/global ((t (:background ,"black" :inverse-video nil))))
   `(sml/folder ((t (:foreground ,badger-charcoal))))
   `(sml/filename ((t (:foreground ,badger-salmon :weight normal))))
   `(sml/prefix   ((t (:foreground ,badger-salmon :weight normal))))
   `(sml/line-number ((t (:foreground ,badger-blue :weight normal))))  
   `(sml/col-number ((t (:foreground ,badger-green :weight normal))))
   `(sml/read-only ((t (:foreground ,badger-charcoal))))
   `(sml/outside-modified ((t (:foreground ,badger-red))))
   `(sml/modified ((t (:foreground ,badger-red))))
   `(sml/remote ((t (:foreground ,badger-charcoal))))
   `(sml/numbers-separator ((t (:foreground ,badger-charcoal))))
   ;;`(sml/client ((t (:foreground ,badger-succ))))
   ;;`(sml/not-modified ((t (:foreground ,badger-yellow))))
   `(sml/git  ((t (:foreground ,badger-blue))))
   `(sml/vc-edited  ((t (:foreground ,badger-blue))))
   `(sml/modes ((t (:foreground ,badger-pink))))
   `(sml/position-percentage ((t (:foreground ,badger-charcoal))))

   `(flyspell-incorrect ((t (:underline (:color ,badger-red :style wave)))))
   `(flyspell-duplicate ((t (:underline (:color ,badger-yellow :style wave)))))
   
   ))


(badger/with-color-variables
  (custom-theme-set-variables
   'badger
   `(ansi-color-names-vector [,"black" ,"#E2434C" ,"#86B187" ,"#E0D063" ,"#84C452" ,"#E18CBB" ,"#8AC6F2" ,"white"])))

;;;###autoload
(when (and (boundp 'custom-theme-load-path) load-file-name)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'badger)


;;; badger-theme.el ends here
    
