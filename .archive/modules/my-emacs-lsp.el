(use-package eglot
  :config
  (add-hook 'prog-mode-hook
	    (lambda ()
              (unless (eq major-mode 'emacs-lisp-mode)
		(flymake-mode)
		(eglot-ensure))))
  ;; Display messages when idle, without prompting
  (setq help-at-pt-display-when-idle t)
  :custom
  (eglot-autoshutdown t)
  (eglot-inlay-hints-mode nil)
  (eglot-command-history t)
  (eglot-cache-session-completions t))

(use-package flyspell
  :config
  (flyspell-mode 1)
(setq flyspell-prog-mode t))

(use-package flymake
  :config
  (setq flymake-suppress-zero-counters t)
  (setq flymake-start-on-flymake-mode t)
  (setq flymake-no-changes-timeout nil)
  (setq flymake-start-on-save-buffer t)
  (setq flymake-show-diagnostics-at-end-of-line nil))

(use-package yasnippet
  :config
  (yas-global-mode 1))

(use-package editorconfig)
(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package eldoc
  :custom
  (eldoc-idle-delay 0))

(use-package eldoc-box
  :config
  ;; (add-hook 'prog-mode-hook
  ;; 	    (lambda ()
  ;; 	      (eldoc-box-hover-mode 1)))

  (global-set-key (kbd "C-h .") 'eldoc-box-help-at-point)
  )

;; Additional Lisp Support
(use-package sly)

;; CSS Support
(use-package scss-mode)
(use-package css-mode)

(use-package go-projectile)

;; Go Support
(use-package go-mode
  :config
  (add-hook 'go-mode-hook #'eglot-ensure))

;; Rust Support
(require 'my-rust)

;; Haskell Support
(use-package haskell-mode)

;; Markdown Support
(use-package markdown-mode)

;; Nix Support
(require 'my-nix)

;; Typescript Support
(use-package typescript-mode)

;; YAML Support
(use-package yaml-mode)

;;; LaTeX support
;; (use-package auctex
;;   :config
;;   (setq TeX-auto-save t)
;;   (setq TeX-parse-self t))

;; Enable LaTeX math support
(add-hook 'LaTeX-mode-map #'LaTeX-math-mode)

;; Enable reference mangment
(add-hook 'LaTeX-mode-map #'reftex-mode)

;;; Debugger 
(use-package dap-mode
  :config
  (dap-ui-mode)
  (dap-ui-controls-mode 1)

  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  ;; installs .extension/vscode
  (dap-gdb-lldb-setup)
  (dap-register-debug-template
   "Rust::LLDB Run Configuration"
   (list :type "lldb"
	 :request "launch"
	 :name "LLDB::Run"
	 :gdbpath "rust-lldb"
	 :target nil
	 :cwd nil)))

(provide 'my-emacs-lsp)
