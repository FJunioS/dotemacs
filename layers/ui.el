;;; ui.el ---  desc  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'core-lib)
(require 'core-packages)

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
;; (when (member "Fantasque Sans Mono" (font-family-list))
;;   (set-frame-font "Fantasque Sans Mono-16" t t))
(when (member "Fantasque Sans Mono" (font-family-list))
  (set-frame-font "Fantasque Sans Mono-14" t))

;; Unicode
(csetq use-default-font-for-symbols nil)
(set-fontset-font t 'unicode (face-attribute 'default :family))
(set-fontset-font t 'unicode "Symbola" nil 'append)
;; Emojis
(set-fontset-font t '(#x2300 . #x27e7) "Twemoji")
(set-fontset-font t '(#x2300 . #x27e7) "Noto Color Emoji" nil 'append)
(set-fontset-font t '(#x27F0 . #x1FAFF) "Twemoji")
(set-fontset-font t '(#x27F0 . #x1FAFF) "Noto Color Emoji" nil 'append)

(custom-set-faces
 '(default ((t :family "Fantasque Sans Mono")))
 '(fixed-pitch ((t :family "Fantasque Sans Mono")))
 '(mode-line ((t (:family "DejaVu Sans Mono"))))
 '(variable-pitch ((t (:family "Noto Serif")))))

(add-hook 'org-mode-hook (lambda () (variable-pitch-mode t)))
(add-hook 'markdown-mode-hook (lambda () (variable-pitch-mode t)))

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
  :elpaca (:host github :repo "honmaple/emacs-maple-minibuffer")
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
  (csetq completion-in-region-function (kind-icon-enhance-completion completion-in-region-function)
         kind-icon-extra-space t)
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package modus-themes
  :defer 0
  :preface
  (defun set-theme ()
    "Set theme and config."
    (interactive)
    (load-theme 'modus-vivendi-tinted :no-confirm)
    (font-lock-add-keywords 'emacs-lisp-mode
                            '(("'\\|#'\\|\\." . 'font-lock-constant-face)
                              ("(\\|)" . 'font-lock-punctuation-face)
                              ("[0-9]" . 'font-lock-number-face)))
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

  :config
  (require 'modus-themes)
  (general-after-gui
    (set-theme)))

(general-after-gui
  (defvar mode-line-align-left nil)
  (defvar mode-line-align-middle nil)
  (defvar mode-line-align-right nil)
  (defconst RIGHT_PADDING 1)
  (setq mode-line-align-left
        '(("%e" mode-line-front-space
           (:propertize
            ("" mode-line-mule-info mode-line-client mode-line-modified mode-line-remote)
            display (min-width (5.0)))
           mode-line-frame-identification
           mode-line-buffer-identification
           "   " mode-line-position
           "  " )))

  (setq mode-line-align-middle
        '(""
          (:propertize (:eval (symbol-name major-mode)) face font-lock-type-face)
          (eglot--managed-mode
           (" [" eglot--mode-line-format "] "))
          " "
          ))

  (csetq mode-line-align-right
         '(""
           (:propertize (:eval (shorten-directory default-directory 30)) face font-lock-string-face)
           (vc-mode vc-mode)
           " | "
           flymake-mode-line-format
           " | "
           mode-name
           "\t"
           (global-mode-string
            ("" global-mode-string))
           ))

  (defun mode-line-fill-right (face reserve)
    "Return empty space using FACE and leaving RESERVE space on the right."
    (unless reserve
      (setq reserve 20))
    (when (and window-system (eq 'right (get-scroll-bar-mode)))
      (setq reserve (- reserve 3)))
    (propertize " "
                'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
                'face face))

  (defun mode-line-fill-center (face reserve)
    "Return empty space using FACE to the center of remaining space leaving RESERVE space on the right."
    (unless reserve
      (setq reserve 20))
    (when (and window-system (eq 'right (get-scroll-bar-mode)))
      (setq reserve (- reserve 3)))
    (propertize " "
                'display `((space :align-to (- (+ center (.5 . right-margin)) ,reserve
                                               (.5 . left-margin))))
                'face face))

  (defun reserve-left/middle ()
    (/ (length (format-mode-line mode-line-align-middle)) 2))

  (defun reserve-middle/right ()
    (+ RIGHT_PADDING (length (format-mode-line mode-line-align-right))))

  (csetq mode-line-format
         (list
          mode-line-align-left
          '(:eval (mode-line-fill-center 'mode-line
                                         (reserve-left/middle)))
          mode-line-align-middle
          '(:eval
            (mode-line-fill-right 'mode-line
                                  (reserve-middle/right)))
          mode-line-align-right)))

(provide 'ui)
;;; ui.el ends here
