;; -*- lexical-binding: t; -*-
(require 'core-packages)

;;; Tools:
(add-hook 'prog-mode-hook #'column-number-mode)
(defconst use-eglot t)

(use-package tempel
  :init
  ;; Setup completion at point
  (setq tempel-template-sources 'tempel-path-templates)
  (setq tempel-path (concat emacs-dir "templates"))

  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)
  (defun +tempel-complete-or-end ()
    (interactive)
    (if tempel--active (call-interactively #'tempel-done) (call-interactively #'tempel-complete)))

  :bind
  (:map global-map
        ("M-t" . #'+tempel-complete-or-end)
        :map org-mode-map
        ("M-t" . #'+tempel-complete-or-end))

  :preface
  (defun l (item-list)
    "Receive a list and make the user select one item."
    (if-let ((selected-item (completing-read "Select item: " item-list)))
        selected-item
      nil))

  ;; TODO: Move this to lib
  (defun empty-line ()
    "Returns true in case current line is empty"
    (when (save-excursion (re-search-backward "^\\S-*$" (line-beginning-position) 'noerror))
      t)))

(use-package yasnippet
  :disabled t
  :init (yas-global-mode 1)
  :custom
  (yas-snippet-dirs `(,(concat emacs-dir "snippets/")))
  :config
  (add-hook 'rust-mode-hook
            (lambda ()
              (setq yas-buffer-local-condition
                    yas-not-string-or-comment-condition))))

(use-package ellama
  :init
  (setopt ellama-language "English")
  (require 'llm-ollama)
  (setopt ellama-provider
          (make-llm-ollama
           :chat-model "zephyr"
           :embedding-model "zephyr")))

(use-package eldoc
  :after elpaca-after-init-hook
  :ensure t
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
  :ensure (:host github :repo "jdtsmith/indent-bars")
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
  :if use-eglot
  :after eglot
  :init (global-flycheck-eglot-mode))

(use-package rainbow-mode
  :init (rainbow-mode)
  :config
  ;; remove highlighting color names (useful only in CSS)
  (setq rainbow-x-colors nil))

(use-package project
  :no-require t
  :ensure nil
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

  (map global-map
       "C-t C-r" #'projectile-recentf
       "C-t C-f" #'projectile-find-file
       "C-t C-g" #'+projectile-ripgrep)

  (defun +projectile-ripgrep ()
    (interactive)
    (consult-ripgrep projectile-project-root))

  (pushnew! projectile-globally-ignored-directories
            "^\\.cache$" "tmp" "^target$")

  (projectile-mode +1))

(use-package tree-sitter-langs
  :after tree-sitter)

(use-package tree-sitter
  :config
  (require 'tree-sitter-langs)
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(global-hi-lock-mode 1)
(setq hi-lock-file-patterns-policy #'(lambda (dummy) t))

(use-package eglot
  :if use-eglot
  :bind (:map eglot-mode-map
              ("C-h ." . eldoc))
  :hook ((eglot-managed-mode . my/eglot-eldoc-settings))
  :config
  (defun my/eglot-eldoc-settings ()
    (csetq eldoc-documentation-strategy
           'eldoc-documentation-compose-eagerly))

  (after! cape
    (defun my/eglot-capf ()
      (setq-local completion-at-point-functions
                  (list (cape-capf-super
                         'cape-file))))

    (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
    (advice-add 'eglot-completion-at-point :around #'cape-wrap-noninterruptible))

  (setq completion-category-overrides '((eglot (styles orderless))))
  (setq eglot-ignored-server-capabilites '(:inlayHintProvider)))

(use-package lsp-mode
  :unless use-eglot
  :commands lsp
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))) ;; Configure orderless
  :hook
  (lsp-completion-mode . my/lsp-mode-setup-completion)
  (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
   (rustic-mode . lsp)
   (rust-mode . lsp)
   ;; if you want which-key integration
   (lsp-mode . lsp-enable-which-key-integration)))
(use-package jsonrpc)
(use-package jinja2-mode)


(use-package lsp-ui
  :hook
  (lsp-ui-mode . lsp-mode))
(use-package web-mode)
(use-package haskell-mode)
(use-package dape
  :ensure (:host github :repo "svaante/dape"))

(use-package restclient
  :ensure t)

;;
;;; Languages:
(use-package sml-mode)
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
  :mode "\\.rs\\'"
  :init
  (when use-eglot
    (remove-hook 'rustic-mode-hook 'flycheck-mode)
    (csetq rustic-setup-eglot t
           rustic-lsp-client 'eglot))

  (csetq rustic-compile-directory-method 'rustic-buffer-workspace
         rustic-enable-detached-file-support t
         rustic-format-on-save nil
         rustic-cargo-check-exec-command "clippy"
         rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer")
         rustic-cargo-check-arguments "-- -W clippy::pedantic -W clippy::nursery -W clippy::unwrap_used"))

(provide 'langs)
;;; langs.el ends here
