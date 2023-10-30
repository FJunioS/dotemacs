;;; my-nix.el --- nix configuration

(use-package nix-mode
    :defer t
    :mode "\\.nix\\'"
    :config
    (electric-indent-mode -1))

(provide 'my-nix)
;;; my-nix.el ends here
