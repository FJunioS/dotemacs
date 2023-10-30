;;; my-emacs-git.el --- Version Control management
(require 'my-org)
(use-package magit)
(use-package magit-svn)
(use-package forge)
(use-package ediff)

(setq vc-diff-switches '("-b" "-B" "-u"))
(setq vc-git-diff-switches nil)

;; Verbose commit screen
(setq magit-omit-untracked-dir-contents t)

(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(global-set-key (kbd "M-s") 'magit-status)
(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
(define-key magit-status-mode-map (kbd "P") 'magit-push)
(define-key magit-svn-mode-map    (kbd "P") 'magit-svn-dcommit)

(defun magit-exit-commit-mode ()
  (interactive)
  (kill-buffer)
  (delete-window))

(eval-after-load "git-commit-mode"
  '(define-key git-commit-mode-map (kbd "C-c C-k") 'magit-exit-commit-mode))

(defun magit-commit-mode-init ()
  (when (looking-at "\n")
    (open-line 1)))
(defadvice git-commit-commit (after delete-window activate)
  (delete-window))

(add-hook 'git-commit-mode-hook 'magit-commit-mode-init)

;; Colorful diff mode
(setq magit-diff-refine-hunk 'all)

;;; Automatically enable Magit Svn (for submodules)
(add-hook 'magit-mode-hook (lambda()
                             (require 'magit-svn)
                             (if (magit-svn-get-ref-info)
                                 (magit-svn-mode))))

(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)

(provide 'my-emacs-git)
;;; my-emacs-git.el ends here
