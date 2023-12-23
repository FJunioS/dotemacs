;c;; langs.el ---  desc  -*- lexical-binding: t; -*-
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
  (setq eldoc-idle-delay 0.5
        eldoc-current-idle-delay 0.5)
  (add-to-list 'display-buffer-alist
               '("\\*eldoc\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 80)
                 (window-parameters
                  (no-delete-other-windows . t)))))

(use-package eldoc-box
  :after eldoc)

(use-package devdocs
  :ensure t
  :init
  (add-hook 'rust-mode-hook (lambda () (setq-local devdocs-current-docs '("rust")))))

(use-package indent-bars
  :elpaca (:host github :repo "jdtsmith/indent-bars")
  :custom
    (indent-bars-color '(highlight :face-bg t :blend 0.2))
    (indent-bars-pattern ".")
    (indent-bars-highlight-current-depth nil)
    (indent-bars-width-frac 0.1)
    (indent-bars-pad-frac 0.1)
    (indent-bars-zigzag nil)
    (indent-bars-color-by-depth nil)
    (indent-bars-display-on-blank-lines nil)
    (indent-bars-treesit-support t)
    (indent-bars-no-descend-string t)
    (indent-bars-treesit-ignore-blank-lines-types '("module"))
  :ensure t
  :hook ((rustic-mode rust-mode toml-mode yaml-mode lua-mode) . indent-bars-mode))

(use-package flycheck
  :init (global-flycheck-mode)
  :config
  ;; Avoid "No file or directory" errors when requiring local packages
  (eval-and-compile
    (setq flycheck-emacs-lisp-load-path load-path))

  (setq flycheck-display-errors-delay 0.01
         flycheck-idle-change-delay 0.01
         flycheck-idle-buffer-switch-delay 0.01))

(use-package flycheck-eglot
  :ensure t
  :after eglot
  :init (global-flycheck-eglot-mode))

(use-package rainbow-mode
  :init (rainbow-mode)
    :ghook 'prog-mode-hook 'text-mode-hook
    :config
    ;; remove highlighting color names (useful only in CSS)
    (setq rainbow-x-colors nil))

(use-package project
  :no-require t
  :ensure t
  :elpaca nil
  :init
  (csetq project-vc-extra-root-markers '(".env" "Cargo.toml" "justfile"))
  (defvar project-root-markers '("Cargo.toml" "justfile")
    "Files or directories that indicate the root of a project.")

  (defun aorst/project-find-root (path)
    "Tail-recursive search in PATH for root markers."
    (require 'transient)
    (let* ((this-dir (file-name-as-directory (file-truename path)))
           (parent-dir (expand-file-name (concat this-dir "../")))
           (system-root-dir (expand-file-name "/")))
      (cond
       ((aorst/project-root-p this-dir) (cons 'transient this-dir))
       ((equal system-root-dir this-dir) nil)
       (t (aorst/project-find-root parent-dir)))))

  (defun aorst/project-root-p (path)
    "Check if current PATH has any of project root markers."
    (let ((results (mapcar (lambda (marker)
                             (file-exists-p (concat path marker)))
                           project-root-markers)))
      (eval `(or ,@ results))))
  (add-hook 'project-find-functions 'aorst/project-find-root))

(use-package projectile
  :disabled
  :ensure t
  :defer 0
  :commands (projectile-project-root
             projectile-project-name
             projectile-locate-dominating-file
             projectile-relevant-known-projects
             projectile-project-p)
  :config
  (csetq projectile-auto-discover nil
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
  :bind (:map eglot-mode-map
              ("C-h ." . eldoc))
  :hook ((eglot-managed-mode . my/eglot-eldoc-settings))
  :config
  (defun my/eglot-eldoc-settings ()
    (csetq eldoc-documentation-strategy
          'eldoc-documentation-compose-eagerly))

  (setq completion-category-overrides '((eglot (styles orderless))))
  (setq eglot-ignored-server-capabilites '(:inlayHintProvider)))

(use-package dape
  :elpaca (:host github :repo "svaante/dape"))

(use-package restclient
  :ensure t)

;;
;;; Languages:

(use-package yuck-mode)
(use-package nushell-mode)
(use-package nushell-ts-mode)
(use-package typescript-mode)
(use-package markdown-mode)
(use-package lua-mode
  :mode "\\.lua\\'")

(use-package nix-mode)
;; rust
(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :init
  (remove-hook 'rustic-mode-hook 'flycheck-mode)
  (csetq rustic-setup-eglot t
         ;; available: rustic-buffer-crate and rustic-project-root
         rustic-compile-directory-method 'rustic-buffer-workspace
         rustic-lsp-client 'eglot
         rustic-enable-detached-file-support t
         rustic-format-on-save nil
         rustic-cargo-check-exec-command "clippy"
         rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer")
         rustic-cargo-check-arguments "-- -W clippy::pedantic -W clippy::nursery -W clippy::unwrap_used"))

(provide 'langs)
;;; langs.el ends here
