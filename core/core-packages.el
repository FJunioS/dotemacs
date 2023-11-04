;;; core-packages.el ---  desc  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar elpaca-installer-version 0.5)
(defvar elpaca-directory (expand-file-name "elpaca/" cache-dir))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(when (>= emacs-major-version 24)
  ;; Install use-package support
  (elpaca elpaca-use-package
    ;; Enable :elpaca use-package keyword.
    (elpaca-use-package-mode)
    ;; Assume :elpaca t unless otherwise specified.
    (setq elpaca-use-package-by-default t)
    (require 'package)
    (setq package-enable-at-startup nil)
    (setq package-archives '())
    (package-initialize)
    (add-to-list 'package-archives
                 '("melpa" . "https://melpa.org/packages/") t)
    ))


(require 'core-window)
(core-handle-popup (rx "*elpaca-log*"))
;; don't require `use-package' when loading compiled file; saves a millisecond
;; or 2; compiling now saves ~0.1s overall (maybe another 0.1s after general
;; rewrite)
(eval-when-compile
  (require 'use-package)
  ;; don't actually need `eval-when-compile' for rest since currently loading
  ;; entire init file before compiling already
  (setq use-package-always-defer t))

(elpaca-wait)

(use-package org :elpaca nil)

(use-package blackout :demand t)
(use-package diminish :demand t)
(use-package general
  :demand t
  :ensure t
  :config (require '+general))

(elpaca-wait)

(use-package exwm
  :ensure t
  :config
  ;;https://github.com/ch11ng/exwm/wiki
  (require 'exwm-config)
  (unless (get 'exwm-workspace-number 'saved-value)
    (setq exwm-workspace-number 10))

  (gsetq exwm-workspace-show-all-buffers t)

  (require 'exwm-randr)

  (defconst primary-monitor "HDMI-A-0")
  (defconst secondary-monitor "DisplayPort-0")

  (gsetq exwm-randr-workspace-monitor-plist
        `(0 ,secondary-monitor
          1 ,primary-monitor
          2 ,primary-monitor
          3 ,primary-monitor
          4 ,primary-monitor
          5 ,secondary-monitor
          6 ,secondary-monitor
          7 ,secondary-monitor
          8 ,secondary-monitor))

  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --output HDMI-A-0 --auto --primary --output DisplayPort-0 --auto --below HDMI-A-0")))

  (exwm-randr-enable)

  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  (defun exwm-all-buffers ()
    (seq-filter
     (lambda (buffer)
       (eq 'exwm-mode (buffer-local-value 'major-mode buffer)))
     (buffer-list)))

  (defvar exwm-buffer-source
    `(:name "EXWM"
            :hidden t
            :narrow ?x
            :category buffer
            :state ,#'consult--buffer-state
            :items ,(lambda () (mapcar #'buffer-name (exwm-all-buffers)))))

 (add-to-list 'consult-buffer-sources 'exwm-buffer-source 'append)

  (defvar exwm-global-map--list '())
  (defvar exwm-global--table-map (make-hash-table :test 'equal))

  (defun exwm-global-map ()
    (interactive)
    (let ((map '()))
      (maphash
       (lambda (k v)
         (cl-pushnew `(,k . ,(car v)) map))
       exwm-global--table-map)
      (setq exwm-global-map--list map))
    exwm-global-map--list)

  (defun exwm-global-map::set (hk val)
    (eval `(puthash ,(kbd hk) #',val ,'exwm-global--table-map)))

  (defun exwm-global-map::remove (key)
    (eval `(remhash ,(kbd hk) ,'exwm-global--table-map)))

  (defun wm-map (&rest bindings)
    (progn
      (seq-map (lambda (pair)
                 (exwm-global-map::set (car pair) (cdr pair)))
               (seq-partition bindings 2)))
    (setq exwm-input-global-keys nil)
    (csetq exwm-input-global-keys (exwm-global-map)))

  (defmacro symbol-or! (val)
    `(if ,(ignore-error (symbol-name val))
        (symbol-name ,val)
      ,val))

  (defmacro set-exwm-wm (hk ws)
    (eval
     `(wm-map
       ,`(format "s-%s" (symbol-or! ,hk))
       (lambda () (interactive) (exwm-workspace-switch-create ,ws)))))

  (set-exwm-wm 0 0)
  (set-exwm-wm 1 1)
  (set-exwm-wm 2 2)
  (set-exwm-wm 3 3)
  (set-exwm-wm 4 4)
  (set-exwm-wm 'z 6)
  (set-exwm-wm 'x 5)
  (set-exwm-wm 'c 7)
  (set-exwm-wm 'v 8)

  (defmacro first-arg! (arg)
      `(if (string-match ,`(format "\\b\\(%s\\)\\b" ,arg) ,arg)
          (match-string 1 ,arg)))

(defun vertico-buffer-window-other-frame (buffer)
"Return window pointer if BUFFER is visible in another frame.

If BUFFER is visible in the current frame, return nil"
       (with-current-buffer (window-buffer (selected-window))
         (if (and (derived-mode-p 'exwm-mode)
                  exwm--floating-frame)
             ;; Switch from a floating frame.
             (with-current-buffer buffer
               (if (and (derived-mode-p 'exwm-mode)
                        exwm--floating-frame
                        (eq exwm--frame exwm-workspace--current))
                   ;; Switch to another floating frame.
                   (frame-root-window exwm--floating-frame)
                 ;; Do not switch if the buffer is not on the current workspace.
                 (or (get-buffer-window buffer exwm-workspace--current)
                     (selected-window))))
           (with-current-buffer buffer
             (when (derived-mode-p 'exwm-mode)
               (if (eq exwm--frame exwm-workspace--current)
                   (when exwm--floating-frame
                     ;; Switch to a floating frame on the current workspace.
                     (frame-selected-window exwm--floating-frame))
                 ;; Do not switch to exwm-mode buffers on other workspace (which
                 ;; won't work unless `exwm-layout-show-all-buffers' is set)
                 (unless exwm-layout-show-all-buffers
                   (selected-window))))))))

  (defun core-exwm-shortcut (key command)
    (wm-map key
            `(lambda ()
               (interactive)
               (start-process-shell-command ,`(first-arg! ,command) nil ,command))))

  (core-exwm-shortcut "s-d" "rofi -modi drun,run -show drun")
  (core-exwm-shortcut "s-S" "flameshot gui")
  (core-exwm-shortcut "s-D" "discord")

  (wm-map "s-r" #'exwm-reset
          "s-m" #'exwm-workspace-move-window)

  (wm-map "s-q" #'delete-window
          "s-d" (lambda () (interactive) exwm-workspace-switch)
          "s-s" #'exwm-workspace-switch)

  (wm-map "s-&"
          (defun exwm-shell-command (command)
            (interactive (list (read-shell-command "$ ")))
            (let ((default-directory "~"))
              (start-process "exwm-subproc" nil
                             "bash" "-c" "nohup bash -c \"$1\" >/dev/null 2>&1" "--" command))))

  (ad-activate 'set-window-buffer)

  (exwm-input--key [s--] #'exwm-workspace-switch)


  (add-hook 'exwm-update-class-hook
            (lambda ()
              (exwm-workspace-rename-buffer exwm-class-name)))

  (setq exwm-input-simulation-keys
        '(([?\C-b] . [left])
          ([?\C-f] . [right])
          ([?\C-j] . [up])
          ([?\C-k] . [down])
          ([?\C-p] . [up])
          ([?\C-n] . [down])
          ([?\C-a] . [home])
          ([?\C-e] . [end])
          ([?\C-u] . [prior])
          ([?\C-d] . [next])
          ([?\C-k] . [S-end delete])))

  ;; avoid closing emacs without meaning to
  (global-set-key (kbd "C-x C-c") nil)

  (setq save-interprogram-paste-before-kill t)

  (setq window-divider-default-right-width 1)
  (window-divider-mode)

  (exwm-enable))

(use-package desktop-environment
  :ensure t)

(use-package exwm-edit
  :ensure t
  :config
  (defun ag-exwm/on-exwm-edit-compose ()
    (spacemacs/toggle-visual-line-navigation-on)
    (funcall 'markdown-mode))

  (add-hook 'exwm-edit-compose-hook 'ag-exwm/on-exwm-edit-compose))

;; Launch apps with completion on point interface
(use-package app-launcher
  :after exwm
  :config
  (wm-map "s-d" #'app-launcher-run-app)
  :elpaca '(app-launcher :host github :repo "SebastienWae/app-launcher"))


(use-package pinentry
  :ensure t
  :config
    (setq epg-gpg-program "gpg2")  ;; not necessary
    (require 'epa-file)
    (epa-file-enable)
    (setq epa-pinentry-mode 'loopback)
    (setq epg-pinentry-mode 'loopback)

    (setenv "GPG_AGENT_INFO" nil)  ;; use emacs pinentry
    (setq auth-source-debug t)
    (pinentry-start)

    (require 'org-crypt)
    (org-crypt-use-before-save-magic)
    )

(provide 'core-packages)
;;; core-packages.el ends here
