;; -*- lexical-binding: t; -*-

(require 'core-lib)
(require 'core-packages)

(use-package beacon
  :ensure t
  :hook (beacon-mode . prog-mode-hook))

(use-package doom-modeline
  :init (doom-modeline-mode 1))
(defvar default-font "Iosevka Comfy")
(defvar fixed-pitch-font "Fantasque Sans Mono")
(defvar variable-pitch-font "Iosevka Aile")

;; Define Main Font Families
(set-face-attribute 'default nil :family default-font)
(set-face-attribute 'fixed-pitch nil :family fixed-pitch-font)
(set-face-attribute 'variable-pitch nil :family variable-pitch-font)
;; … Set face families
(set-face-attribute 'mode-line nil :family variable-pitch-font)
;; Define height
(set-face-attribute 'default nil :height 160)

;; Avoid Emacs set default font to apply on symbols
;; This may be useful only for Variable Pitch fonts (Noto Sans, FreeSerif, …)
(csetq-default use-default-font-for-symbols nil)

(set-fontset-font t 'unicode (face-attribute 'default :family))
(dolist (font '("Symbola"
                   "Symbols Nerd Font"
                   "Twemoji"
                   "Noto Color Emoji"
                   "Noto Sans Mro"
                   "Noto Sans SignWriting"
                   "FreeSerif"))
  (when (member font (font-family-list))
    (set-fontset-font t 'unicode font nil 'append)))
(set-fontset-font t 'unicode (font-spec :family "Noto Sans") nil 'append)

(use-package emojify
  :hook (elpaca-after-init-hook . global-emojify-mode)
  :bind
  (:map ju-menu-map
        ("." . #'emoji-search))
  :custom
  (emojify-display-style 'unicode)
  (emojify-emoji-styles '(unicode)))

(use-package fontaine
  :config
  (csetq fontaine-presets `((small
                             :default-family ,fixed-pitch-font
                             :default-height 80
                             :variable-pitch-family ,variable-pitch-font)
                            (regular)
                            (medium
                             :default-weight semilight
                             :default-height 115
                             :bold-weight extrabold)
                            (large
                             :inherit medium
                             :default-height 150)
                            (presentation
                             :inherit medium
                             :default-weight light
                             :default-height 180)
                            (t
                             :default-family ,default-font
                             :default-weight regular
                             :default-height 160
                             :fixed-pitch-family ,fixed-pitch-font
                             :fixed-pitch-weight nil ; use default weight
                             :fixed-pitch-height 1.0
                             :variable-pitch-family ,variable-pitch-font
                             :variable-pitch-weight nil
                             :variable-pitch-height 1.0
                             :bold-family nil
                             :bold-weight bold
                             :italic-family nil
                             :italic-slant italic
                             :line-spacing nil)
                            (default))))

(csetq winner-boring-buffers-regexp " \*Minibuf-[0-9]+")
;; Internal border


;; Line spacing, can be 0 for code and 1 or 2 for text
(setq-default line-spacing 0)
(add-hook 'text-mode-hook (lambda () (setq line-spacing 1)))

(tool-bar-mode -1)
(scroll-bar-mode -1)

(modify-all-frames-parameters
 '((right-divider-width . 40)
   (vertical-scroll-bars . nil)
   (internal-border-width . 24)))
(dolist (face '(window-divider
                window-divider-first-pixel
                window-divider-last-pixel))
  (face-spec-reset-face face)
  (set-face-foreground face (face-attribute 'default :background)))
(set-face-background 'fringe (face-attribute 'default :background))
;; No Tooltips
(tooltip-mode 0)
(csetq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 org-ellipsis "▾"

 ;; Agenda styling
 org-agenda-tags-column 0
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "<── now ─────────────────────────────────────────────────")
;; Underline line at descent position, not baseline position
(setq x-underline-at-descent-line t)

;; Fringes only in programming mode
(add-hook 'prog-mode-hook (defun! ju//fringe-mode-set ()
                            (fringe-mode '(13 . 0))))

;; Vertical window divider
(setq window-divider-default-right-width 3)
(setq window-divider-default-places 'right-only)
(window-divider-mode)

(with-eval-after-load 'org
  (setq org-display-inline-images t)
  (setq org-redisplay-inline-images t)
  (setq org-startup-with-inline-images "inlineimages")
  (setq org-hide-emphasis-markers t)
  (setq org-link-elisp-confirm-function nil))

;; Nice glyphs for truncated and wrapped lines
(defface face-faded nil
  "Oo." :group 'koi)
(defface fallback '((t :family "Fantasque Mono Sans"
                       :inherit 'face-faded)) "Fallback.")
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'fallback))
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code ?↩ 'fallback))
(set-display-table-slot standard-display-table 'selective-display
                        (string-to-vector " …"))

;; No ugly button for checkboxes
(csetq widget-image-enable nil)

;; No sound
(setq visible-bell t)
(setq ring-bell-function 'ignore)

(csetq indicate-empty-lines t)

(+handle-popup (rx "Output*" eol))
(+handle-popup (rx "*Warnings*") t)
(+handle-popup (rx "*eldoc*") t)
(+handle-popup (rx bol "*elpaca-" (* any)) t)

(csetq scroll-conservatively 1000)
(csetq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

(add-hook! '(text-mode-hook prog-mode-hook)
  (lambda () (size-indication-mode)))

;; No limit on font lock
(csetq font-lock-maximum-size nil)

;; Stop prompting about your true desire of killing innocent processes
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; No confirmation for visiting non-existent files
(setq confirm-nonexistent-file-or-buffer nil)

;; Mouse active in terminal
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  (global-set-key (kbd "<mouse-4>") 'scroll-down-line)
  (global-set-key (kbd "<mouse-5>") 'scroll-up-line))

;; Minimum window height
(setq window-min-height 1)

;; Size of temporary buffers
(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)

;; Buffer encoding
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment   'utf-8)

;; Unique buffer names
(require 'uniquify)
(csetq uniquify-buffer-name-style 'reverse
       uniquify-separator " • "
       uniquify-after-kill-buffer-p t
       uniquify-ignore-buffers-re "^\\*")

;; Kill term buffer when exiting
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg))
  (if (memq (process-status proc) '(signal exit))
      (let ((buffer (process-buffer proc)))
        ad-do-it
        (kill-buffer buffer))
    ad-do-it))
(ad-activate 'term-sentinel)

;; ** Font
;; (set-fontset-font t #xFE0F spec-1)                ;; Variation Selector 16
;; (set-fontset-font t '(#x1F1E6 . #x1F1FF) spec-2)  ;; Regional Indicator Syms
;; (set-fontset-font t '(#x1F3FB . #x1F3FF) spec-3)  ;; Emoji Modifiers
;; (set-fontset-font t '(#x1F700 . #x1F77F) spec-4)  ;; Alchemical Symbols
(use-package mixed-pitch
  :hook
  ;; Acts on all modes for text editing (including org, markdown, etc.).
  (text-mode . mixed-pitch-mode))

  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8-unix)
  (setq emojify-display-style 'unicode)
  (setq emojify-emoji-styles '(unicode))
  (when (member "Fantasque Sans Mono" (font-family-list))
    (set-frame-font "Fantasque Sans Mono 14")
    (add-to-list 'default-frame-alist
                 '(font . "Fantasque Mono Sans 14"))
    (face-remap-add-relative 'fixed-pitch :family "Fantasque Mono Sans"))



  (use-package ligature
    :ensure t
    :init
    (ligature-set-ligatures 't '("www"))
    ;; Enable ligatures in programming modes
    (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\"
                                         "\\\\\\" "{-" "::" ":::" ":=" "!!" "!="
                                         "!==" "-}" "----" "-->" "->" "->>" "-<"
                                         "-<<" "-~" "#{" "#[" "##" "###" "####"
                                         "#(" "#?" "#_" "#_(" ".-" ".=" ".."
                                         "..<" "..." "?=" "??" ";;" ";;;" "/*"
                                         "/**" "/=" "/==" "/>" "//" "///" "&&"
                                         "||" "||=" "|=" "|>" "^=" "$>" "++" "+++"
                                         "+>" "=:=" "==" "===" "==>" "=>" "=>>"
                                         "<=" "<<" "=/=" ">-" ">=" ">=>" ">>"
                                         ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
                                         "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                         "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<"
                                         "<<-" "<<=" "<<<" "<~" "<~~" "</" "</>"
                                         "~@" "~-" "~>" "~~" "~~>" "%%"))
    (global-ligature-mode 1))

  (use-package highlight-defined
    :ensure t
    :init
    (highlight-defined-mode 1))

  (use-package mini-echo
    :disabled
    :custom
    (mini-echo-right-padding 10)
    :init
    (setq hl-line-sticky-flag nil)
    (setq global-hl-line-sticky-flag nil)
    (setq mini-echo-separator "  ")
    ;; set default segments of long/short style
    (setq-default mini-echo-default-segments
                  '(:long  ("time" "major-mode" "eglot" "vcs" "buffer-position"
                            "flymake" "process" "narrow" "macro" "profiler"
                            "meow" "buffer-name" "buffer-size" "selection-info")
                           :short ("buffer-name-short" "meow" "buffer-position" "process"
                                   "profiler" "selection-info" "narrow" "macro")))
    (mini-echo-mode))

  (use-package highlight-escape-sequences
    :after 'modus-themes
    :init
    (general-after-gui
      (hes-mode)))

  (use-package maple-minibuffer
    :ensure (:host github :repo "honmaple/emacs-maple-minibuffer")
    :config
    (setq maple-minibuffer:position-type 'frame-bottom-center
          maple-minibuffer:border-color
          (or (ignore-errors (ewal-get-color 'foreground) "gray50"))
          maple-minibuffer:height nil
          maple-minibuffer:width 0.7
          maple-minibuffer:cache t))

  (setq echo-keystrokes 1)

  ;; Icons
  (use-package all-the-icons)
  (use-package all-the-icons-completion
    :init
    (all-the-icons-completion-mode)
    :config
    (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

  (use-package kind-icon
    :ensure t
    :after corfu
    ;;  :custom
    ;;  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
    :config
    (csetq completion-in-region-function
           (kind-icon-enhance-completion
            (lambda (&rest args)
              (apply (if vertico-mode
                         #'consult-completion-in-region
                       #'completion--in-region)
                     args))))
    (setq kind-icon-extra-space t)
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

  (use-package modus-themes
    :preface
    (defun set-theme ()
      "Set theme and config."
      (interactive)
      (load-theme 'modus-vivendi-tinted :no-confirm)
      (font-lock-add-keywords 'emacs-lisp-mode
                              (list
                               (cons (rx (or (and bow "'") "#'" " . ")) 'font-lock-constant-face)
                               '("(\\|)" . 'font-lock-punctuation-face)
                               (cons (rx (and bow "?" (not "?"))) 'font-lock-punctuation-face)
                               '("[0-9]" . 'font-lock-number-face)))
      (set-face-attribute
       'font-lock-constant-face nil
       :foreground "#f9b37f")

      (custom-set-faces
       ;; custom-set-faces was added by Custom.
       ;; If you edit it by hand, you could mess it up, so be careful.
       ;; Your init file should contain only one such instance.
       ;; If there is more than one, they won't work right.
       '(custom-variable-tag ((t (:inherit bold :foreground "medium sea green"))))
       '(dired-directory ((t (:foreground "SkyBlue1"))))
       '(diredp-file-name ((t (:foreground "light blue"))))
       '(flymake-end-of-line-diagnostics-face ((t (:inherit modus-themes-slant
                                                            :box (:line-width (1 . 1) :color "#61647a")
                                                            :height 0.85))))
       '(font-lock-punctuation-face ((t (:foreground "#8c7ec7"))))
       '(fringe ((t (:background "#101d20"))))
       '(whitespace-trailing ((t (:foreground "#9d1f1f"))))
       '(show-paren-match-expression ((t (:box nil :background "#2c273a"))))
       '(flymake-error-echo-at-eol ((t (:foreground "#ff5f59"))))
       '(modus-themes-lang-warning ((t (:underline "#f9b37f"))) t)
       '(cursor ((t (:background "#4b93ab"))))
       '(flymake-note-echo-at-eol ((t (:foreground "#6ae4b9" :box nil))))
       '(font-lock-warning-face ((t (:inherit modus-themes-bold :foreground "violet"))))
       '(whitespace-line ((t (:foreground "gainsboro" :underline (:color "RoyalBlue4" :style wave :position nil)))))
       '(whitespace-space-after-tab ((t (:inherit whitespace-space)))))

      (with-eval-after-load 'hl-line-mode
        (set-face-attribute
         'hl-line nil
         :background "#2c273a"
         :box '(:line-width (-1 . -1):color "#1d1f24" :style 'flat)))

      (set-face-attribute 'fringe nil
                          :background "#101d20")

      (set-face-background 'default "#1d1f24")

      (set-face-attribute 'font-lock-comment-face nil
                          :slant 'italic)

      (set-face-attribute 'font-lock-keyword-face nil
                          :weight 'bold)

      (with-eval-after-load 'eglot
        (set-face-attribute 'eglot-highlight-symbol-face nil
                            :underline t
                            :bold nil))

      (set-face-attribute 'font-lock-number-face nil
                          :foreground "#f9b37f")

      (with-eval-after-load 'hl-line-mode
        (set-face-attribute
         'highlight-numbers-number nil
         :foreground "#f9b37f"))

      (set-face-attribute
       'font-lock-regexp-grouping-backslash nil
       :foreground "#f9b37f" :bold nil)

      (set-face-attribute
       'font-lock-regexp-grouping-construct nil
       :foreground "#86b2d3" :bold t)

      (set-face-attribute
       'font-lock-regexp-face nil
       :foreground "#e0def4" :bold t)

      (set-face-attribute ;; comment for test
       'font-lock-comment-face nil
       :foreground "#9c7ec7")

      (with-eval-after-load 'org-mode
        (set-face-attribute ;; comment for test
         'org-hide nil
         :foreground "#8c7ec7"))

      (set-face-attribute
       'font-lock-string-face nil
       :foreground "#a3bbae")

      (set-face-attribute
       'font-lock-builtin-face nil
       :foreground "#bab4fe")

      (set-face-attribute
       'font-lock-keyword-face nil
       :foreground "#bab4fe")

      (set-face-attribute
       'font-lock-function-name-face nil
       :foreground "#97DED7")

      (set-face-attribute
       'show-paren-match nil
       :background "unspecified" :bold t
       :box '(:line-width (-1 . -1) :color "#f8a4b6" :style 'flat)))
    :init
    (require 'modus-themes)
    (ju-after-gui
      (set-theme)))

  (ju-after-gui
    (setq mode-line-format nil)

    (kill-local-variable 'mode-line-format)

    (force-mode-line-update)


    (setq-default mode-line-format
                  '("%e"
                    my-modeline-buffer-name
                    "%&"
                    "  "
                    my-modeline-major-mode))

    (defface my-modeline-background
      '((t :background "#3355bb" :foreground "white" :inherit bold))
      "Face with a red background for use on the mode line.")

    (defun my-modeline--buffer-name ()
      "Return `buffer-name' with spaces around it."
      (format " %s " (buffer-name)))

    (defvar-local my-modeline-buffer-name
        '(:eval
          (when (mode-line-window-selected-p)
            (propertize (my-modeline--buffer-name) 'face 'my-modeline-background)))
      "Mode line construct to display the buffer name.")

    (put 'my-modeline-buffer-name 'risky-local-variable t)

    (defun my-modeline--major-mode-name ()
      "Return capitalized `major-mode' as a string."
      (capitalize (symbol-name major-mode)))

    (defvar-local my-modeline-major-mode
        '(:eval
          (list
           "\tL%l,C%c\t"
           (propertize "λ" 'face 'shadow)
           " "
           (propertize (my-modeline--major-mode-name) 'face 'bold)))
      "Mode line construct to display the major mode.")

    (put 'my-modeline-major-mode 'risky-local-variable t)

    ;; Emacs 29, check the definition right below
    (mode-line-window-selected-p)

    (defun mode-line-window-selected-p ()
      "Return non-nil if we're updating the mode line for the selected window.
This function is meant to be called in `:eval' mode line
constructs to allow altering the look of the mode line depending
on whether the mode line belongs to the currently selected window
or not."
      (let ((window (selected-window)))
        (or (eq window (old-selected-window))
	          (and (minibuffer-window-active-p (minibuffer-window))
	               (with-selected-window (minibuffer-window)
	                 (eq window (minibuffer-selected-window))))))))

  (provide 'ui)
;;; ui.el ends here

(provide 'ui)
