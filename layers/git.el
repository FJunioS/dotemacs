(require 'core-packages)

(use-package magit
  :defer 3
  :config
  (general-after-gui
    (noct-handle-popup-same-window magit-status-mode)
    (noct-handle-popup-same-window magit-log-mode)
    (noct-handle-popup-same-window magit-cherry-mode)
    (noct-handle-popup-same-window magit-log-select-mode)
    (noct-handle-popup-other-window magit-revision-mode)
    (noct-handle-popup-other-window-no-select magit-diff-mode))
  :general
  (leader/vcs "g" #'magit))

(use-package forge)

(use-package git-commit
:config
;; so `fill-paragraph' works correctly for bullet points
(setq git-commit-major-mode 'org-mode)
(general-add-hook 'git-commit-mode-hook
                  #'auto-fill-mode))

(provide 'git)
;; git.el ends here
