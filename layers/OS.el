;;; OS.el ---  desc  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;;; NixOS
(defun nixos-rebuild ()
  "Rebuild the nixos system."
  (interactive)
  (display-buffer
   (get-buffer-create "*nixos-rebuild*")
   '((display-buffer-below-selected display-buffer-at-bottom)
     (inhibit-same-window . t)))
  (start-process "nixos-rebuild" "*nixos-rebuild*" "sudo"
                 "nixos-rebuild" "switch"))

;;; OS.el ends here
