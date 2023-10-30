;;; my-eshell.el --- eshell configuration
(use-package eshell
  :hook ('eshell-first-time-mode . 'my-eshell-config)
  :config
  (setq eshell-history-size 5000
	eshell-buffer-maximum-lines 5000
	eshell-hist-ignoredups t
	eshell-scroll-to-bottom-on-input t
	eshell-destroy-buffer-when-process-dies t
	eshell-visual-commands'("fish" "htop" "ssh" "top" "zsh" "bash")))

(use-package eshell-git-prompt)

(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

(defun my-eshell-config ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Bind some useful keys for evil-mode
  (setq eshell-history-size     10000
	eshell-buffer-maximum-lines 10000
	eshell-hist-ignoredups t
	eshell-scroll-to-bottom-on-input t))

(use-package vterm)

(provide 'my-eshell)
;;; my-eshell.el ends here
