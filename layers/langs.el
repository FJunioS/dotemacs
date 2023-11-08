;;; langs.el ---  desc  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'core-packages)

;;
;;; Tools:
(add-hook 'prog-mode-hook #'column-number-mode)
(electric-pair-mode 1)

(use-package ellama
  :init
  (setopt ellama-language "English")
  (require 'llm-ollama)
  (setopt ellama-provider
          (make-llm-ollama
           :chat-model "zephyr"
           :embedding-model "zephyr")))

(use-package eldoc
  :ghook 'elpaca-after-init-hook
  :config
  (setq eldoc-display-functions '(eldoc-display-in-echo-area  eldoc-display-in-buffer))
  (setq eldoc-idle-delay 0.02
        eldoc-current-idle-delay 0.01))

(use-package eldoc-box
  :after eldoc)

;; (use-package flycheck
;;   :init
;;   (global-flycheck-mode)
;;   :config
;;   (setq flycheck-display-errors-delay 0.01
;;          flycheck-idle-change-delay 0.01
;;          flycheck-idle-buffer-switch-delay 0.01))

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
  (leader/file
   "." #'projectile-find-file)
  (leader/project
    "a" #'projectile-add-known-project
    "p" #'projectile-switch-project
    "c" #'projectile-compile-project)
  :config
  (gsetq projectile-auto-discover nil
         projectile-sort-order 'recently
         projectile-enable-caching (not noninteractive)
         projectile-project-search-path '("~/dev/"))

  (dolist (dir '("^~\\.cache$" "^/tmp$" "^target$"))
    (setq projectile-globally-ignored-directories
          (append dir projectile-globally-ignored-directories)))
  (projectile-mode +1))

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
