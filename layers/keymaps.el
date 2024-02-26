;; -*- lexical-binding: t; -*-

;;; keymaps.el ---  desc  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'core-packages)
(require 'core-lib)

(map global-map "C-s" #'isearch-forward-regexp)

(define-key input-decode-map (kbd "C-[") [control-bracketleft])
(define-key input-decode-map (kbd "ESC") [escape])

;; Disable arrow keys to force me to use emacs navigation
(global-map "<left>" nil
            "<right>" nil
            "<up>" nil
            "<down>" nil)

(global-map "C-," #'execute-extended-command
            "C-x C-s" #'manual-save-buffer
            "M-<right>" #'next-buffer
            "M-<left>"  #'previous-buffer)

;; I keep mistakenly using this more than I should
(global-map "C-x C-l" nil
            "C-x C-n" nil
            "C-c c C-l" #'downcase-region
            "C-c c C-n" #'set-goal-column)

(global-map "<escape>" #'escape
            "C-h =" #'describe-char)

;;-------------------------------------------------
;;                Leader map
;; ------------------------------------------------

;;; Define 'leader-map'
(create-keymap leader-map)
(global-map "C-t" leader-map)

(create-keymap ju-toggle-map)
(create-keymap ju-search-map)
(create-keymap ju-emacs-map)
(create-keymap ju-git-map)
(create-keymap ju-buffer-map)
(create-keymap ju-help-map)
(create-keymap ju-mode-map)
(create-keymap ju-open-map)
(create-keymap ju-agenda-map)
(create-keymap ju-window-map)
(create-keymap ju-project-map)
(create-keymap ju-notes-map)
(create-keymap ju-bookmark-map)
(create-keymap ju-code-map)
(create-keymap ju-rectangle-map)
(create-keymap ju-menu-map)

(map leader-map
     "a" (cons "Agenda" ju-agenda-map)
     "b" (cons "Buffer" ju-buffer-map)
     "c" (cons "Code" ju-code-map)
     "e" (cons "Emacs" ju-emacs-map)
     "g" (cons "Git" ju-git-map)
     "m" (cons "Mode" ju-mode-map)
     "x" (cons "Menu" ju-menu-map)
     "n" (cons "Notes" ju-notes-map)
     "h" (cons "Help" ju-help-map)
     "o" (cons "Open" ju-open-map)
     "p" (cons "Project" ju-project-map)
     "r" (cons "Bookmark" ju-bookmark-map)
     "s" (cons "Search" ju-search-map)
     "T" (cons "Toggle" ju-toggle-map)
     "w" (cons "Window" ju-window-map)

     "R" (cons "Rectangle" ju-rectangle-map))

;; default
(map leader-map
     "TAB" 'mode-line-other-buffer
     "*" #'delete-window
     "(" #'delete-other-windows
     "-" #'split-window-below
     "v" #'split-window-right
     "k" #'+kill-window)

(map global-map
     "C-S-c" #'compile
     "C-S-r" #'recompile
     "C-c k" #'+kill-window
     "C-c *" #'delete-window
     "C-c (" #'delete-other-windows
     "C-c -" #'split-window-below
     "C-c v" #'split-window-right
     "C-c TAB" 'mode-line-other-buffer)

(map ju-buffer-map
     "k" #'+kill-this-buffer
     "i" #'ibuffer
     "b" #'switch-to-buffer)

(map ju-open-map
     "c" (cons "Emacs Config" (lambda () (interactive) (consult-fd emacs-dir))))

(map ju-help-map
     "a" #'apropos-command
     "v" #'helpful-variable
     "f" #'helpful-function
     "s" #'helpful-symbol
     "i" #'consult-info)

;; bookmark
(map ju-bookmark-map
     "b" #'consult-bookmark
     "a" #'bookmark-set)

(map ju-open-map
     "t" #'eat
     "e" (cons "Emacs Config dir" (lambda () (interactive) (consult-fd emacs-dir))))

(map ju-rectangle-map
     "c" #'delete-whitespace-rectangle
     "d" #'delete-rectangle
     "e" #'rectangle-exchange-point-and-mark
     "i" #'copy-rectangle-to-register
     "k" #'kill-rectangle
     "l" #'rectangle-left-char
     "m" #'rectangle-mark-mode
     "n" #'rectangle-next-line
     "N" #'rectangle-number-lines
     "o" #'open-rectangle
     "p" #'rectangle-previous-line
     "r" #'rectangle-right-char
     "s" #'string-rectangle
     "t" #'string-rectangle
     "x" #'clear-rectangle
     "y" #'yank-rectangle)

(map ju-menu-map
     "<tab>" #'mode-line-other-buffer
     "b" #'ibuffer
     "c" (cmds! (equal major-mode 'emacs-lisp-mode) #'eval-defun)
     "d" #'dirvish
     "k" #'+kill-window
     "h" #'consult-buffer
     "g" #'consult-ripgrep
     "r" #'consult-recent-file
     "s" #'manual-save-buffer
     "t" #'find-file
     "e" (cmds! (equal major-mode 'emacs-lisp-mode) #'pp-eval-last-sexp))

(provide 'keymaps)
;;; keymaps.el ends here


(provide 'keymaps)
;;; keymaps.el ends here.
