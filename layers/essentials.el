;;; essentials.el --- Variables and main user settings  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'cl-lib)
(require 'popup-handler)
(require 'core-packages)
(require '+evil)
(require 'general)

(use-package keychain-environment)

(use-package circe
  :config
(setq circe-network-options
      '(("Libera Chat"
         :tls t
         :nick "Juniju"
         :channels ("#emacs-circe" "#emacs")
         ))))

;; http://yummymelon.com/devnull/customizing-the-emacs-tools-menu.html
(easy-menu-add-item global-map '(menu-bar tools)
                    ["Magit Status"
                     magit-status
                     :visible (vc-responsible-backend default-directory)
                     :help "Show the status of the current Git repository in a buffer"]
                     "Version Control")


(use-package dashboard
  :ensure t
  :demand t
  :defer 0
  :general
  (leader/open
   "d" #'(dashboard-open :wk "Dashboard"))
  (general-def 'normal 'dashboard-mode-map
    ;; Widgets
    "r" (general-simulate-key "r") ; recent
    "m" (general-simulate-key "m") ; bookmarks
    "p" (general-simulate-key "p") ; projects
    "a" (general-simulate-key "a") ; agenda
    "e" (general-simulate-key "e") ; registers
    ;; Movement
    "j" 'widget-forward
    "k" 'widget-backward
    "J" 'dashboard-next-line ; move between items and empty lines
    "K" 'dashboard-previous-line
    [tab] 'dashboard-next-section
    [backtab] 'dashboard-previous-section

    ;; Other commands
    [down-mouse-1] 'widget-button-click)
  :config
  (add-hook 'dashboard-mode-hook #'ju/dashboard-mode-hook--visual-adjustments)
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents   . 5)
                          (bookmarks . 5)
                          (projects  . 5)
                          (agenda    . 5)))

  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
  :preface
  (defun ju/dashboard-mode-hook--visual-adjustments nil
    (hl-line-mode)
    (visual-fill-column-mode)
    (gsetq-local visual-fill-column-width 70)))

(use-package savehist
  :defer 0
  :elpaca nil
  :config
  (savehist-mode 1)
  (save-place-mode 1)
  (add-hook 'kill-emacs-hook #'savehist-save)

  (add-function :after after-focus-change-function
                #'savehist-save)

  (setq savehist-additional-variables
        '(mark-ring
          global-mark-ring
          compile-command
          compile-history
          compilation-directory
          shell-command-history
          search-ring
          regexp-search-ring
          extended-command-history)
        savehist-file (concat cache-dir "savehist-backup")
        savehist-save-minibuffer-history t)

  (setq auto-save-default t
        auto-save-include-big-deletions t
        auto-save-list-file-prefix (expand-file-name "autosave/" cache-dir)
        auto-save-file-name-transforms
        (list (list "\\`/[^/]*:\\([^/]*/\\)*\\([^/]*\\)\\'"
                    ;; Prefix tramp autosaves to prevent conflicts with local ones
                    (concat auto-save-list-file-prefix "tramp-\\2") t)
              (list ".*" auto-save-list-file-prefix t))))

(general-with-package 'compile
  (require 'general)
  (setq
   ;; save modified buffers without asking
   compilation-ask-about-save nil
   compilation-scroll-output 'first-error)

  ;; http://stackoverflow.com/a/20788581/2744245
  (defun my-colorize-compilation-buffer ()
    (require 'ansi-color)
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (general-add-hook 'compilation-filter-hook #'my-colorize-compilation-buffer)
  (noct-handle-popup compilation-mode))

(require 'zone-words)

(use-package vimish-fold
  :config
  (vimish-fold-global-mode))

(use-package apropos
  :elpaca nil
  :init
  ;; TODO can this be put in :config?
  (noct-handle-popup apropos-mode)
  :config
  (setq apropos-do-all t)
  (general-def apropos-mode-map "q" #'quit-window))

(use-package helpful
  :general
  (general-def help-map
    "RET" #'helpful-at-point
    "f" #'helpful-callable
    "v" #'helpful-variable
    "k" #'helpful-key
    "o" #'helpful-symbol)
  :init
  ;; using this instead of binding them directly allows taking an alternate action
  ;; without also opening the helpful buffer
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable)
  :config
  (general-def helpful-mode
    :definer 'minor-mode
    "q" #'quit-window)
  (noct-handle-popup help-mode)
  (noct-handle-popup (rx "*Help*"))
  (noct-handle-popup helpful-mode))

(use-package gcmh
  :ghook ('pre-command-hook nil nil nil t)
  :config
  ;; settings used by doom; default infinite threshold causes Emacs to
  ;; completely freeze after working for a while; maybe this will be better
  (setq gcmh-idle-delay 10
        gcmh-high-cons-threshold 16777216)
  (general-add-hook 'focus-out-hook #'gcmh-idle-garbage-collect))

(use-package recentf
  :elpaca nil
  :ghook 'elpaca-after-init-hook
  :init (recentf-mode)
  :general
  ("C-x C-r" #'recentf)
  (leader/file
    "r" #'recentf)
  :config
  (setq recentf-max-saved-items 1000)
  (setq recentf-exclude '("^/tmp/emacs/.*" "^/var/folders\\.*" "COMMIT_EDITMSG\\'"
                           ".*-autoloads\\.el\\'" "[/\\]\\.elpa/"))
  (add-hook! 'kill-emacs-hook #'recentf-save-list))

(use-package clipetty
  :ensure t
  :init
  ;; only need to load if create a terminal frame
  ;; `global-clipetty-mode' will not cause issues if enabled for a server with
  ;; both graphical and terminal frames
  (general-after-tty
    (global-clipetty-mode)))

(use-package which-key
  :defer 1
  :ghook ('pre-command-hook nil nil nil t)
  :general
  (leader/toggle "W" #'which-key-mode)
  ;; replace `where-is'; don't need because can show in M-x
  (help-map "w" #'which-key-show-top-level)
  :init
  ;; should be set before loading
  (setq which-key-idle-delay 0.3)
  :config
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-prefix-then-key-order
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-sort-uppercase-first nil)

  (which-key-mode))

(use-package ibuffer
  :elpaca nil
  :init
  (require '+ibuffer)
  :general
  (leader/buffer "i" #'ibuffer)
  ('ibuffer-mode-map
   "<tab>" #'ibuffer-toggle-filter-group
   "q" #'kill-this-buffer)
  :config
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")
              (ibuffer-update nil t)
              (ibuffer-auto-mode 1)))

  (setq ibuffer-use-header-line t
        ibuffer-expert t
        ibuffer-show-empty-filter-groups nil))

(use-package undo-tree
  :general (nmap "U" #'undo-tree-visualize)
  (general-def 'normal 'undo-tree-visualizer-mode-map
    "k" #'undo-tree-visualize-undo
    "j" #'undo-tree-visualize-redo)
  (general-def 'motion
    "j" #'evil-next-visual-line
    "k" #'evil-previous-visual-line)
  :diminish undo-tree-mode
  :init (global-undo-tree-mode)
  :config
  (setq evil-undo-system 'undo-tree)
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        undo-tree-history-directory-alist (eval `'(("." . ,cache-dir)))))

(use-package vterm
  :preface
  (defun vterm-ins ()
    "Insert character before cursor."
    (interactive)
    (vterm-goto-char (point))
    (vterm-reset-cursor-point)
    (call-interactively #'evil-insert))
  :init (setq vterm-always-compile-module t)
  :general
  (general-def 'insert 'vterm-mode-map
    "C-a" 'vterm--self-insert
    "C-b" 'vterm--self-insert     ; Should not be necessary.
    "C-d" 'vterm--self-insert
    "C-e" 'vterm--self-insert
    "C-f" 'vterm--self-insert     ; Should not be necessary.
    "C-k" 'vterm--self-insert
    "C-l" 'vterm--self-insert     ; Should not be necessary.
    "C-n" 'vterm--self-insert
    "C-o" 'vterm--self-insert
    "C-p" 'vterm--self-insert
    "C-q" 'vterm--self-insert     ; Should not be necessary.
    "C-r" 'vterm--self-insert
    "C-s" 'vterm--self-insert     ; Should not be necessary.
    "C-t" 'vterm--self-insert
    "C-u" 'vterm--self-insert     ; Should not be necessary.
    "C-v" 'vterm--self-insert     ; Should not be necessary.
    "C-w" 'vterm--self-insert
    "C-y" 'vterm--self-insert
    "C-z" 'vterm--self-insert
    "<delete>" 'vterm-send-delete)

  (general-def 'normal 'vterm-mode-map
    "i" #'vterm-ins
    "u" #'vterm-undo
    "P" #'vterm-yank
    "]]" 'vterm-next-prompt
    "[[" 'vterm-previous-prompt
    "G" #'vterm-reset-cursor-point
    "RET" #'vterm-send-return)
  :config

  (general-with 'evil
    (general-def 'insert 'vterm-mode-map
      "<escape>" (lookup-key evil-insert-state-map (kbd "<escape>"))))

  (setq vterm-shell "fish"
        ;; increase scrollback
        vterm-max-scrollback 10000
        vterm-use-vterm-prompt-detection-method t)
  (noct-handle-popup "*vterm*"))

(use-package vterm-toggle
  :general
  (leader/open "t" #'vterm-toggle)
  (general-def 'normal 'vterm-mode-map
    "q" #'vterm-toggle-hide))

(provide 'essentials)
;;; essentials.el ends here
