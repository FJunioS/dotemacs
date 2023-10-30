;;; my-emacs-dired.el --- Dired settings ;; -*- lexical-binding: t; -*-
;;; Commentary:
;; none

(use-package dired
	:straight (:type built-in)
    :custom
    (dired-listing-switches "-laF")
    :config
    (setq find-ls-option '("-print0 | xargs -0 ls -ldF" . "-ld"))

    ;; Hide details on dired startup
    (defadvice dired-readin
		(after dired-after-updating-hook first () activate)
		(let ((dired-details-internal-overlay-list  ())) (dired-hide-details-mode))))

(use-package dired+
	:custom
	(diredp-toggle-find-file-reuse-dir t))
(use-package dired-du)
(use-package dired-subtree
	:config
	(define-key dired-mode-map (kbd "S-TAB") 'dired-subtree-revert)
	(define-key dired-mode-map (kbd "TAB") 'dired-subtree-toggle))
(use-package peep-dired
	:bind (:map peep-dired-mode-map
			  ("SPC" . nil)
			  ("<backspace>" . nil)))

(save-place-mode +1)
(global-auto-revert-mode +1)

(use-package projectile
	:config
	(progn
		(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
		(setq projectile-completion-system 'default)
		(setq projectile-enable-caching t)
		(setq projectile-indexing-method 'alien)
		(add-to-list 'projectile-globally-ignored-files "node_modules")
		(add-to-list 'projectile-globally-ignored-files ".git")
		(add-to-list 'projectile-globally-ignored-files ".cache")
		(add-to-list 'projectile-globally-ignored-files "_cache")
		))

(provide 'my-emacs-dired)
;;; my-emacs-dired.el ends here
