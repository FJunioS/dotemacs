;; -*- lexical-binding: t; -*-

;; Linux
(defun ju/system-sleep ()
  (interactive)
  (save-all-history)
  (call-process "systemctl" nil nil nil "suspend"))
(defalias 'ju/system-suspend 'ju/system-sleep)

(defun ju/keymap-dvorak ()
  (interactive)
  (call-process "setxkbmap" nil nil nil "-variant" "dvp"))

(defun ju/keymap-qwerty ()
  (interactive)
  (call-process "setxkbmap" nil nil nil "-variant" ","))

(defun ju/system-shutdown ()
  (interactive)
  (save-all-history)
  (call-process "systemctl" nil nil nil "poweroff"))

(defun ju/system-logout ()
  (interactive)
  (save-all-history)
  (call-process "pkill" nil nil nil
                "-KILL" "-u" (user-login-name)))

;; NixOS
(defun nixos-rebuild ()
  "Rebuild the nixos system."
  (interactive)
  (display-buffer
   (get-buffer-create "*nixos-rebuild*")
   '((display-buffer-below-selected display-buffer-at-bottom)
     (inhibit-same-window . t)))
  (start-process "nixos-rebuild" "*nixos-rebuild*" "sudo"
                 "nixos-rebuild" "switch"))

(provide 'OS)
;;; os.el ends here.
