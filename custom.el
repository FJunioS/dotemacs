;;; custom.el --- Custom variables and Emacs "don't ask me again" things

(put 'magit-clean 'disabled nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2628939b8881388a9251b1bb71bc9dd7c6cffd5252104f9ef236ddfd8dbbf74a" "5bafdfa3e21f921abf9b9fd77e1e0ce032e62e3a6f8f13ec8ce7945727c654e9" default))
 '(package-selected-packages
   '(zoxide vertico projectile popper pinentry orderless hydra general embark-consult devdocs desktop-environment cape async)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(cursor ((t (:background "#4b93ab"))))
 '(custom-variable-tag ((t (:inherit bold :foreground "medium sea green"))))
 '(dired-directory ((t (:foreground "SkyBlue1"))))
 '(diredp-file-name ((t (:foreground "light blue"))))
 '(fixed-pitch ((t :inherit :family "Fantasque Sans Mono")))
 '(flymake-end-of-line-diagnostics-face ((t (:inherit modus-themes-slant :box (:line-width (1 . 1) :color "#61647a") :height 0.85))))
 '(flymake-error-echo-at-eol ((t (:foreground "#ff5f59"))))
 '(flymake-note-echo-at-eol ((t (:foreground "#6ae4b9" :box nil))))
 '(font-lock-punctuation-face ((t (:foreground "#8c7ec7"))))
 '(font-lock-warning-face ((t (:inherit modus-themes-bold :foreground "violet"))))
 '(fringe ((t :background "white")))
 '(header-line ((t :box (:line-width 4 :color "grey90" :style nil))))
 '(header-line-highlight ((t :box (:color "black"))))
 '(keycast-key ((t)))
 '(line-number ((t :background "white")))
 '(mode-line ((t :inherit :family "DejaVu Sans Mono")))
 '(mode-line-active ((t :box (:line-width 4 :color nil :style nil))))
 '(mode-line-highlight ((t :box (:color "black"))))
 '(mode-line-inactive ((t :box (:line-width 4 :color "grey90" :style nil))))
 '(modus-themes-lang-warning ((t (:underline "#f9b37f"))) t)
 '(show-paren-match-expression ((t (:box nil :background "#2c273a"))))
 '(tab-bar-tab ((t :box (:line-width 4 :color "grey85" :style nil))))
 '(tab-bar-tab-inactive ((t :box (:line-width 4 :color "grey75" :style nil))))
 '(variable-pitch ((t :inherit :family "Noto Serif")))
 '(whitespace-line ((t (:foreground "gainsboro" :underline (:color "RoyalBlue4" :style wave :position nil)))))
 '(whitespace-space-after-tab ((t (:inherit whitespace-space))))
 '(whitespace-trailing ((t (:foreground "#9d1f1f"))))
 '(window-divider ((t :background "white" :foreground "white")))
 '(window-divider-first-pixel ((t :background "white" :foreground "white")))
 '(window-divider-last-pixel ((t :background "white" :foreground "white"))))
