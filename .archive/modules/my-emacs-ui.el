;;; my-emacs-ui.el --- User Interface modifications
;; Load font settings

;; Colorful brackets for better text handling
;; (use-package rainbow-delimiters
;;   :hook (prog-mode . rainbow-delimiters-mode))

(setq default-cursor-color "blue")
(setq yasnippet-can-fire-cursor-color "purple")

;; Highlight line
(add-hook 'prog-mode-hook 'hl-line-mode)

;; Fringe (space between frame and text) -- 20px left, 0 right
(add-hook 'prog-mode-hook (lambda () (fringe-mode '(20 . 0))))

;; Visualize indent guides
(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-auto-character-face-perc 100)
  (set-face-foreground
   'highlight-indent-guides-character-face "dimgray")
  (setq highlight-indent-guides-method 'bitmap))

;; Column Line limit
(add-hook 'prog-mode-hook
	  (lambda () (display-fill-column-indicator-mode 90)))

(defun my-setup-color-theme ()
      (interactive)
  (when (display-graphic-p)
	  (load-theme 'modus-vivendi :no-confirm)))

(my-setup-color-theme)

(provide 'my-emacs-ui)
;;; my-emacs-ui.el ends here
