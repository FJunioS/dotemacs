;; -*- lexical-binding: t; -*-

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq lock-file-n
      `((".*" ,temporary-file-directory t))) 

;; ** Mouse settings
(setq-default smooth-scroll-margin 0)
(setq   vc-follow-symlinks t)
(setq   mouse-yank-at-point t)

;; Electric 🔌
(defun toggle-electric-mode ()
  (interactive)
  (electric-pair-mode)
  (electric-quote-mode)
  (electric-layout-mode))

(unless (electric-pair-mode)
  (toggle-electric-mode))

(use-package alert
  :config
  (setq alert-default-style 'growl))

;; (use-package smart-mode-line
;; 	:config
;; 	(setq sml/theme 'automatic)
;; 	(add-to-list
;; 		'sml/replacer-regexp-list
;; 		'("^~/sync/emacs/" ":Emacs:"
;; 			 "^/sudo:.*:" ":SUDO:"
;; 			 "^~/sync/" ":Sync:"))
;; 	:init
;; 	(smart-mode-line-enable))
	(setq mode-line-position (list "%I %l:%c\t%p"))

(use-package easy-kill
	:config
	(global-set-key (kbd "C-M-SPC") 'easy-mark))

(global-prettify-symbols-mode +1)

(delete-selection-mode t)
(setq cursor-type 'box)  ;; (box, bar, hbar)

(setq delete-pair-blink-delay 0.15)       ; Emacs28 
(setq help-window-select nil)               ; don't go to help window automatically
(setq next-error-recenter '(4))           ; center of the window
(setq find-library-include-other-files nil) ; Emacs 29
(setq remote-file-name-inhibit-delete-by-moving-to-trash t) ; Emacs 30
(setq remote-file-name-inhibit-auto-save t)                 ; Emacs 30

;; Use UTF-8 ㊙️
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(setq whitespace-display-mappings
      '((newline-mark ?\n [?¬ ?\n]))) ;; ↵ ¶ ¯ ¬ °
;(set-face-attribute 'whitespace-newline :foreground 'f0f0f0)

;; ** Mouse Wheel behavior
(setq mouse-wheel-scroll-amount
      '(1
        ((shift) . 5)
        ((meta) . 0.5)
        ((control) . text-scale))
      mouse-drag-copy-region nil
      make-pointer-invisible t
      mouse-wheel-progressive-speed t
      moutse-wheel-follow-mouse t)

;; ** Show current time
(setq display-time-load-average nil)
(setq display-time-interval 1)
(setq display-time-24hr-format t)
(setq display-time-format ":: %H:%M:%S :: %d/%m/%y |")
(setq display-time-day-and-date t)
(display-time-mode +1)

;; ** Bookmarks
(setq bookmark-automatically-show-annotations t)
(setq bookmark-fringe-mark nil) ; Emacs 29 to hide bookmark fringe icon
(add-hook 'bookmark-bmenu-mode-hook #'hl-line-mode)

(use-package helpful)

(defun bookmark-save-no-prompt (&rest _)
	(funcall 'bookmark-save))

(advice-add 'bookmark-set-internal :after 'bookmark-save-no-prompt)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control)))
    mouse-wheel-progressive-speed nil
    scroll-conservatively 10
    scroll-preserve-screen-position 1
    fast-but-imprecise-scrolling t)

;; Set default mode of the scratch buffer to Org
(require 'my-org)
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message "\
#+TITLE: Scratch
#+STARTUP: inlineimages
# This buffer is for notes you don't want to save. You can use
# org-mode markup (and all Org's goodness) to organise the notes.
# If you want to create a file, visit that file with C-x C-f,
# then enter the text in that file's own buffer.

[[~/Pictures/dolphin.png]]

*Elisp*
#+begin_src elisp

#+end_src

*Notes:*
-")

(require 'my-eshell)
(require 'my-ibuffer)

(provide 'my-emacs-essential)
;;; my-emacs-essencial.el ends here
