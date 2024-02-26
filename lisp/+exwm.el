;;; +exwm.el ---  desc  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'exwm)
(require 'exwm-config)
(require 'exwm-randr)
(require 'exwm-systemtray)

;; 
;;; Utilities
(defvar exwm-ws-hook nil
  "Hook run right after user switch workspaces, useful for defining local keymaps.")

;; -----------------------------------
;; Monitor settings

(defun exwm-global-map::set (hk val)
  (eval `(puthash ,(kbd hk) ,val ,'exwm-global--table-map)))

(defun exwm-global-map::remove (key)
  (eval `(remhash ,(kbd hk) ,'exwm-global--table-map)))

(defun exwm-all-buffers ()
  (seq-filter
   (lambda (buffer)
     (eq 'exwm-mode (buffer-local-value 'major-mode buffer)))
   (buffer-list)))

(defun core-exwm-command (command)
  (start-process-shell-command (first-arg! command) nil command))

(defun core-exwm-shortcut (key command)
  (wm-map key
          `(lambda ()
             (interactive)
             (start-process-shell-command ,`(first-arg! ,command) nil ,command))))

;; 
;;; Monitor and workspaces
(defconst primary-monitor "HDMI-1")
(defconst secondary-monitor "DP-1")

(setq exwm-randr-workspace-monitor-plist (list 1 primary-monitor 2 secondary-monitor))
(add-hook 'exwm-randr-screen-change-hook
          (lambda ()
            (start-process-shell-command
             "xrandr" nil (concat "xrandr --output " primary-monitor " --primary " "--above " secondary-monitor))))
(exwm-randr-enable)

;; hide modeline from floating windows
(add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
(add-hook 'exwm-floating-exit-hook 'exwm-layout-show-mode-line)

(setq exwm-workspace-number 10)

(csetq exwm-workspace-show-all-buffers t)

(csetq exwm-randr-workspace-monitor-plist
   (list 0 secondary-monitor
         1 primary-monitor
         2 primary-monitor
         3 primary-monitor
         4 primary-monitor
         5 secondary-monitor
         6 secondary-monitor
         7 secondary-monitor
         8 secondary-monitor))

(exwm-systemtray-enable)
(setq exwm-systemtray-height 16)

;;
;;; MAP
(defun wm-map (&rest bindings)
  "Set EXWM global maps on each pair of BINDINGS.

Examples:
\(wm-map \"`s-R'\" #'exwm-reset
         \"`s-m'\" #'exwm-workspace-move-window)"
  (require 'async)
     (progn
       (seq-map (lambda (pair)
                  (interactive)
                  (let* ((hk (kbd (car pair)))
                         (command (cadr pair))
                         (associated (assoc hk exwm-input-global-keys)))
                    ;; Check hotkey uniqueness
                    (when associated
                      (unless (equal associated pair)
                        (setq exwm-igk (assoc-delete-all hk exwm-input-global-keys))))
                    (cl-pushnew (cons hk command) exwm-input-global-keys)))
                (seq-partition bindings 2)))
   ;; Make sure the list is applied
   (funcall (get 'exwm-input-global-keys 'custom-set)
            'exwm-input-global-keys exwm-input-global-keys))

(wm-map "s-P" #'exwm-reset
        "s-m" #'exwm-workspace-move-window
        "s-;" #'delete-window
        "s-SPC" #'exwm-floating-toggle-floating
        "s-," #'kill-this-buffer
        "s-d" #'app-launcher-run-app
        "s-<return>" #'eat-other-window
        "s-s" #'exwm-workspace-switch)

(defmacro generate-exwm-select-ws! (index)
  "Return a named function to run `exwm-workspace-switch-create' for INDEX."
  `(defun! ,(intern (format "ju-exwm-switch-%s" index)) ()
     ,(format "Call `exwm-workspace-switch-create' for index %s." index)
     (interactive)
     (exwm-workspace-switch-create ,index)))

(wm-map "s-&" (generate-exwm-select-ws! 1)
        "s-[" (generate-exwm-select-ws! 2)
        "s-{" (generate-exwm-select-ws! 3)
        "s-}" (generate-exwm-select-ws! 4)
        "s-(" (generate-exwm-select-ws! 5)
        "s-=" (generate-exwm-select-ws! 6)
        "s-*" (generate-exwm-select-ws! 7)
        "s-)" (generate-exwm-select-ws! 8)
        "s-+" (generate-exwm-select-ws! 9)
        "s-]" (generate-exwm-select-ws! 0))

(defmacro generate-exwm-move-ws! (index)
"Return a named function to run `exwm-workspace-move-window' for INDEX."
`(defun! ,(intern (format "ju-exwm-move-%s" index)) ()
   ,(format "Call `exwm-workspace-move-window' for index %s." index)
   (interactive)
   (exwm-workspace-move-window ,index)
   (exwm-workspace-switch-create ,index)))

(wm-map "s-%" (generate-exwm-move-ws! 1)
        "s-7" (generate-exwm-move-ws! 2)
        "s-5" (generate-exwm-move-ws! 3)
        "s-3" (generate-exwm-move-ws! 4)
        "s-1" (generate-exwm-move-ws! 5)
        "s-9" (generate-exwm-move-ws! 6)
        "s-0" (generate-exwm-move-ws! 7)
        "s-2" (generate-exwm-move-ws! 8)
        "s-4" (generate-exwm-move-ws! 9)
        "s-6" (generate-exwm-move-ws! 0))

;; 
;;; Keymaps
(core-exwm-shortcut "s-S" "flameshot gui")
(core-exwm-shortcut "s-D" "discord")

(wm-map "s-c"
        (defun exwm-shell-command (command)
          (interactive (list (read-shell-command "$ ")))
          (let ((default-directory "~"))
            (start-process "exwm-subproc" nil
                           "bash" "-c" "nohup bash -c \"$1\" >/dev/null 2>&1" "--" command))))

(defun core/keyboard-settings ()
  (interactive)
  (start-process "exwm-subproc-keyboard" nil
                 "keyboard.sh"))

(core/keyboard-settings)

(general-with 'evil
  (wm-map "s-j" #'evil-window-down
          "s-k" #'evil-window-up
          "s-h" #'evil-window-left
          "s-l" #'evil-window-right
          "s-<tab>" #'windower-switch-to-last-buffer))

(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))

;; Avoid closing emacs without meaning to
(global-set-key (kbd "C-x C-c") nil)

;; Dvorak
(global-set-key (kbd "C-q") nil)
(global-set-key (kbd "C-q C-o") #'manual-save-buffer)
(global-set-key (kbd "C-q C-.") #'eval-last-sexp)

(add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)
(add-hook 'exwm-floating-exit-hook 'exwm-layout-show-mode-line)

(setq save-interprogram-paste-before-kill t)
(setq window-divider-default-right-width 1)
(window-divider-mode)

(general-with 'consult
  (wm-map "M-y" #'consult-yank-from-kill-ring
          "s-b" #'switch-to-buffer)
  (defvar exwm-buffer-source
    `(:name "EXWM"
            :hidden t
            :narrow ?x
            :category buffer
            :state ,#'consult--buffer-state
            :items ,(lambda () (mapcar #'buffer-name (exwm-all-buffers)))))
  (add-to-list 'consult-buffer-sources 'exwm-buffer-source 'append))

(csetq exwm-wm-hook nil)

(defun exwm-mode-map:default ()
  (general-def exwm-mode-map
    "C-c RET" #'exwm-workspace-move-window
    "C-c C-t RET" #'exwm-layout-toggle-mode-line
    "C-c C-t C-f" #'exwm-floating-toggle-floating
    "C-c C-q" #'exwm-input-send-next-key
    "C-c C-k" #'exwm-input-release-keyboard))

(defmacro match-class-name! (name)
  `(and exwm-class-name
        (string= (downcase exwm-class-name) ,(downcase name))))

;; Ctrl+Q will enable the next key to be sent directly
(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

(define-advice exwm-workspace-switch-create
    (:before (_fn &rest _) exwm--record-last-ws-pos)
  (setq exwm-workspace--previous-index exwm-workspace-current-index))
(define-advice exwm-workspace-switch-create
    (:after (_fn &rest _) exwm--set-cursor-pos)
  (if (or (>= exwm-workspace-current-index 5) (= 0 exwm-workspace-current-index))
      (unless (or (>= exwm-workspace--previous-index 5) (= 0 exwm-workspace--previous-index))
        (core-exwm-command "xdotool mousemove 1080 1620"))
    (when (or (>= exwm-workspace--previous-index 5) (= 0 exwm-workspace--previous-index))
      (core-exwm-command "xdotool mousemove 1280 540"))))

(define-advice exwm-workspace-switch-create
    (:after (_fn &rest _) exwm--set-firefox-keymaps)
  (exwm-mode-map:default)
  (if (match-class-name! "firefox")
      (progn
        (csetq exwm-input-simulation-keys
               '(([?\C-j] . [down])
                 ([?\C-k] . [up])
                 ([?\C-h] . [C-h])))
        (general-def exwm-mode-map
          "C-h C-h" (defun! exwm--firefox-C-h()
                      (interactive)
                      (exwm-input--fake-key ?\C-h))
          "C-c C-c" (defun! exwm--firefox-copy-selection ()
                      (interactive)
                      (exwm-input--fake-key ?\C-c)
                      (sleep-for 0.05)
                      (gui-backend-get-selection 'CLIPBOARD 'STRING))
          ))))

(defun save-history ()
  (require 'recentf)
  (require 'savehist)
  (save-some-buffers)
  (recentf-save-list)
  (savehist-save))

(defun suspend-to-sleep ()
  (interactive)
  (save-history)
  (call-process "systemctl" nil nil nil "suspend"))
(wm-map "s-O" #'suspend-to-sleep)

(defun shutdown ()
  (interactive)
  (save-history)
  (call-process "systemctl" nil nil nil "poweroff"))

(defun logout ()
  (interactive)
  (save-history)
  (call-process "pkill" nil nil nil
                "-KILL" "-u" (user-login-name)))

(start-process "exwm-redshift" nil
               "redshift" "-l -23:-46" "-t 6500:2500" "-b 0.8:0.7")

;; (start-process "exwm-ollama" nil
;;                "ollama" "serve")

(provide '+exwm)
;;; +exwm.el ends here
