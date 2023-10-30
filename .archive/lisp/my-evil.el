;;; my-evil.el --- yeah


    ;; improve help on point
    (add-hook 'prog-mode-hook
	    (lambda ()
	        (if (eq major-mode 'emacs-lisp-mode)
		        (define-key evil-normal-state-map (kbd "K") 'helpful-at-point)
		        (define-key evil-normal-state-map (kbd "K")  'eldoc-box-help-at-point))))

    (use-package drag-stuff
        :config
        (drag-stuff-global-mode 1)

        (define-key evil-visual-state-map (kbd "K") 'drag-stuff-up)
        (define-key evil-visual-state-map (kbd "J") 'drag-stuff-down)
        (define-key evil-visual-state-map (kbd "L") 'drag-stuff-right)
        (define-key evil-visual-state-map (kbd "H") 'drag-stuff-left))

    (evil-define-key 'visual global-map (kbd ">") 'my/evil-shift-right)
    (evil-define-key 'visual global-map (kbd "<") 'my/evil-shift-left)
    (define-key evil-normal-state-map (kbd "TAB") 'evilmi-jump-items)

    (define-key evil-normal-state-map (kbd "C-e") 'end-of-line)
    (define-key evil-normal-state-map (kbd "C-d") 'evil-scroll-down)
    (define-key evil-normal-state-map (kbd "C-f") 'right-char)
    (define-key evil-normal-state-map (kbd "C-b") 'left-char)
    (define-key evil-insert-state-map (kbd "C-g") 'evil--quit-and-goto-normal-mode)
    (define-key evil-insert-state-map (kbd "C-d") 'delete-char)
    (define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
    (define-key evil-insert-state-map (kbd "C-k") 'end-previous-line)

    (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

    (define-key evil-normal-state-map (kbd "M-p") 'mark-defun)
    (define-key evil-normal-state-map (kbd "0") 'evil-next-line-1-first-non-blank)
    (define-key evil-normal-state-map (kbd "-") 'end-of-line)
    (define-key evil-visual-state-map (kbd "0") 'evil-next-line-1-first-non-blank)
    (define-key evil-visual-state-map (kbd "-") 'end-of-line)

    (define-key evil-normal-state-map (kbd "fd") 'evil-write-all)
    (define-key evil-normal-state-map (kbd "fr") 'indent-sexp)
    (define-key key-translation-map (kbd "ch") (kbd "C-h"))
    (define-key key-translation-map (kbd "cx") (kbd "C-x"))

    (define-key evil-normal-state-map (kbd "s") nil)
    (define-key evil-normal-state-map (kbd "so") 'kill-other-buffers)
    (define-key evil-normal-state-map (kbd "si") 'evil-buffer)
    (define-key evil-normal-state-map (kbd "sc") 'delete-other-windows)
    (define-key evil-normal-state-map (kbd "sd") 'kill-this-buffer)
    (define-key evil-normal-state-map (kbd "sn") 'cycle-buffer-backward)
    (define-key evil-normal-state-map (kbd "sp") 'cycle-buffer)
    (define-key evil-normal-state-map (kbd "sk") 'evil-window-top)
    (define-key evil-normal-state-map (kbd "sj") 'evil-window-bottom)
    (define-key evil-normal-state-map (kbd "sh") 'evil-window-left)
    (define-key evil-normal-state-map (kbd "sl") 'evil-window-right)

    (evil-global-set-key 'motion "s" nil)
    (evil-global-set-key 'motion "j" 'evil-next-visual-line)
    (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

    (define-key evil-normal-state-map (kbd "C-M-j") 'mc/mmlte--down)
    (define-key evil-normal-state-map (kbd "C-M-k") 'mc/mmlte--up))

  (use-package cycle-buffer) ;; Allow cycling buffer without falling into `scratch' and `*Messages*' buffers

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :config
  (evil-commentary-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(defun evil--quit-and-goto-normal-mode ()
  (interactive)
  (progn
    (evil-normal-state)
    (keyboard-quit)))

(defun my/evil-shift-right ()
  (interactive)
  (evil-shift-right evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore))

(defun my/evil-shift-left ()
  (interactive)
  (evil-shift-left evil-visual-beginning evil-visual-end)
  (evil-normal-state)
  (evil-visual-restore))
(provide 'my-evil)
;;; my-evil.el ends here
