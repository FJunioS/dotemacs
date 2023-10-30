;;; rust.el --- (rust configuration)

(use-package cargo
  :mode "/\\(Cargo.lock\\|\\.cargo/config\\)\\'")

(use-package cargo-mode
  :custom
  (compilation-scroll-output t)
  :config
  (add-hook 'rustic-mode-hook 'cargo-minor-mode))

(use-package toml-mode
  :mode "/\\(Cargo.lock\\|\\.cargo/config\\)\\'")

(use-package ron-mode ;; Rusty Object Notation
  :mode ("\\.ron\\'" . ron-mode)
  :defer t)

(use-package rustic
  :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
  (rustic-format-on-save t)
  (rustic-lsp-client 'eglot)
  (rustic-clippy-arguments '("-- -W clippy::unwrap_used -W clippy::pedantic -W clippy::nursery -D warnings"))
  :config
  (remove-hook 'rustic-mode-hook 'flycheck-mode)
  (add-hook 'rustic-mode-hook
            (lambda () (prettify-symbols-mode)))
  (add-hook 'rustic-mode-hook 'eglot-ensure)
  (add-hook 'rustic-mode-hook #'jun/rustic-mode-hook))

(defun jun/rustic-mode-hook ()
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))

(provide 'my-rust)
;;; rust.el ends here
