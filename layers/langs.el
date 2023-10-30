;;; langs.el ---  desc  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'core-packages)

;;
;;; Tools:

(add-hook 'prog-mode-hook #'column-number-mode)
(electric-pair-mode 1)

(use-package eldoc
  :config
  (setq eldoc-idle-delay 0.02
        eldoc-current-idle-delay 0.01))

(use-package eldoc-box)

(use-package flycheck
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-display-errors-delay 0.01
         flycheck-idle-change-delay 0.01
         flycheck-idle-buffer-switch-delay 0.01))

;(use-package flycheck-eglot
    ;:init (global-flycheck-eglot-mode))

(use-package rainbow-mode
    :init (rainbow-mode)
    :ghook 'prog-mode-hook 'text-mode-hook
    :config
    ;; remove highlighting color names (useful only in CSS)
    (setq rainbow-x-colors nil))

(use-package projectile
  :commands (projectile-project-root
             projectile-project-name
             projectile-locate-dominating-file
             projectile-relevant-known-projects
             projectile-project-p)
  :general
  (leader/f
    "." #'projectile-find-file)
  (leader/project
    "" '(:keymap projectile-command-map))
  :config
  (setq projectile-auto-discover nil
         projectile-enable-caching (not noninteractive)
         projectile-ignored-projects '("~" "~/dev" "~/sync" "~/.config"))
  (projectile-mode +1))

(use-package highlight-numbers
  :ghook 'prog-mode-hook)

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package cape
  :ensure t
  :after eglot
  :preface
  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'cape-file))))
  :config
  (general-add-hook 'eglot-managed-mode-hook #'my/eglot-capf)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-noninterruptible))

(use-package eglot
  :general
  (general-def 'normal 'eglot-mode-map
    "gr" #'xref-find-references
    "gd" #'xref-find-definitions
    "gD" #'xref-find-definitions-other-window
    "K" #'eldoc-doc-buffer
    "C-t" #'xref-pop-marker-stack
    "ga" #'eglot-code-actions)

  :config
  (general-add-hook 'eglot-maned-mode-hook #'evil-normalize-keymaps)
  (setq completion-category-overrides '((eglot (styles orderless))))
  (setq eldoc-display-functions '(eldoc-display-in-buffer))
  (setq eglot-ignored-server-capabilites '(:inlayHintProvider)))

;;
;;; Languages:

(use-package yuck-mode)
(use-package nushell-mode)
(use-package nushell-ts-mode)
(use-package typescript-mode)
(use-package markdown-mode)

;; rust
(use-package rustic
  :gfhook
  '(lambda () (add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1))))
  :init
  (remove-hook 'rustic-mode-hook 'flycheck-mode)
  (setq rustic-setup-eglot t
         rustic-lsp-client 'eglot
         rustic-enable-detached-file-support t
         rustic-format-on-save nil
         rustic-cargo-check-exec-command "clippy"
         rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer")
         rustic-clippy-default-arguments (concat "--benches --tests --all-features"
                                                 " -- -W clippy::pedantic -W clippy::nursery -W clippy::unwrap_used")))

(provide 'langs)
;;; langs.el ends here
