;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'popup-handler)
(require 'core-packages)

(defun save-all-history ()
  "Save all commands that somehow store any kind of history, like savehist or recentf."
  (when recentf-mode
    (recentf-save-list))
  (when savehist-mode
    (savehist-save t)))

;; save hist files after 10 seconds of idle time
;; if not idle, save every 5 minutes
(run-at-active-interval (* 5 60) 10
  (silently! (save-all-history)))

(add-hook 'kill-emacs-hook 'save-all-history)
(display-time-mode)

(csetq display-time-24hr-format t
       display-time-day-and-date t
       display-time-load-average-threshold nil)

(use-package auto-save
  :ensure
  :init
  (auto-save-mode)
  ;; auto-save files after 10 seconds of idle time
  ;; if not idle, save every 5 minutes
  (csetq auto-save-interval (* 5 60)
         auto-save-timeout 10))

(use-package keychain-environment)

(use-package circe
  :config
  (setq circe-network-options
        '(("Libera Chat"
           :tls t
           :nick "Juniju"
           :channels ("#emacs-circe" "#emacs")
           ))))

;; http://yummymelon.com/devnull/customizing-the-emacs-tools-menu.html
(easy-menu-add-item global-map '(menu-bar tools)
                    ["Magit Status"
                     magit-status
                     :visible (vc-responsible-backend default-directory)
                     :help "Show the status of the current Git repository in a buffer"]
                    "Version Control")

(setq savehist-file (concat cache-dir "savehist-backup")
      save-place-file (concat cache-dir "saveplace")
      auto-save-list-file-prefix (expand-file-name "autosave/" cache-dir)
      auto-save-file-name-transforms `((".*" ,auto-save-list-file-prefix t)))

(savehist-mode 1)
(auto-save-mode 1)
(save-place-mode 1)
(csetq history-length 3000 ; from 300
       search-ring-max 150 ; from 16
       mark-ring-max 100
       global-mark-ring-max 100
       regexp-search-ring-max 100
       kmacro-ring-max 100
       history-delete-duplicates t
       savehist-autosave-interval nil
       savehist-save-minibuffer-history t
       savehist-additional-variables
       '(kill-ring                    ; clipboard
         register-alist               ; macros
         mark-ring                    ; marks
         global-mark-ring
         compile-command              ; Compilation
         compile-history
         compilation-directory
         shell-command-history        ; Commands
         extended-command-history
         search-ring                  ; Seaches
         regexp-search-ring))

(csetq-default
 auto-save-default t
 auto-save-include-big-deletions t)

(add-hook! 'savehist-save-hook
  (defun savehist-remove-unprintable-registers-h ()
    "Remove unwriteable registers (e.g. containing window configurations).
Otherwise, `savehist' would discard `register-alist' entirely if we don't omit
the unwritable tidbits."
    ;; Save new value in the temp buffer savehist is running
    ;; `savehist-save-hook' in. We don't want to actually remove the
    ;; unserializable registers in the current session!
    (setq-local register-alist
                (cl-remove-if-not #'savehist-printable register-alist)))

  (defun savehist-unpropertize-variables-h ()
    "Remove text properties from `kill-ring' to reduce savehist cache size."
    (setq kill-ring
          (mapcar #'substring-no-properties
                  (cl-remove-if-not #'stringp kill-ring))
          register-alist
          (cl-loop for (reg . item) in register-alist
                   if (stringp item)
                   collect (cons reg (substring-no-properties item))
                   else collect (cons reg item)))))

(csetq
 ;; save modified buffers without asking
 compilation-ask-about-save nil
 compilation-scroll-output 'first-error)

;; http://stackoverflow.com/a/20788581/2744245
(defun my-colorize-compilation-buffer ()
  (require 'ansi-color)
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))
(add-hook 'compilation-filter-hook #'my-colorize-compilation-buffer)
(noct-handle-popup compilation-mode)

(require 'zone-words)

(use-package vimish-fold
  :config
  (vimish-fold-global-mode))

(use-package 0x0)

(use-package tmr
  :ensure t
  :custom
  (tmr-sound-file (concat emacs-assets-dir "singing-bowl.wav")))

(use-package apropos
  :ensure nil
  :init
  ;; TODO can this be put in :config?
  (noct-handle-popup apropos-mode)
  :config
  (setq apropos-do-all t)
  (define-key apropos-mode-map "q" #'quit-window))

(use-package helpful
  :config
  (map help-map
       "RET" #'helpful-at-point
       "f" #'helpful-callable
       "v" #'helpful-variable
       "k" #'helpful-key
       "o" #'helpful-symbol)

  (noct-handle-popup help-mode)
  (noct-handle-popup (rx "*Help*"))
  (noct-handle-popup helpful-mode))

(use-package emms
  :config
  (emms-all)
  (setq emms-source-file-default-directory "~/Music"
        emms-info-asynchronously t
        emms-browser-covers #'emms-browser-cache-thumbnail-async
        emms-show-format "♪ %s")

  (if (executable-find "mplayer")
      (setq emms-player-list '(emms-player-mplayer))
    (emms-default-players))
  (global-key "M-7" #'emms-smart-browse))

(use-package gcmh
  :init
  ;; settings used by doom; default infinite threshold causes Emacs to
  ;; completely freeze after working for a while; maybe this will be better
  (gcmh-mode 1)
  (setq gcmh-idle-delay 10
        gcmh-high-cons-threshold 16777216)
  (add-hook 'focus-out-hook #'gcmh-idle-garbage-collect))

(use-package recentf
  :ensure nil
  :hook (elpaca-after-init-hook . recentf)
  :init (recentf-mode)
  :custom
  (recentf-save-file (concat cache-dir "recentf"))
  :config
  (global-map "C-x C-r" #'recentf)
  (setq recentf-auto-cleanup nil
        recentf-max-saved-items 200)
  (csetq recentf-exclude '("^/tmp/emacs/.*" ".*/\\.cache/emacs" ".*/\\.rustup" "^/var/folders\\.*" "^/$"
                           "COMMIT_EDITMSG\\'" ".*-autoloads\\.el\\'" "[/\\]\\.elpa/\\'" ".*/bookmarks\\'"
                           "/nix/store/"))

  (setq recentf-auto-cleanup (if (daemonp) 300))
  (add-hook 'kill-emacs-hook #'recentf-save-list)
  (add-hook 'kill-emacs-hook #'recentf-cleanup))

(use-package clipetty
  :ensure t
  :init
  ;; only need to load if create a terminal frame
  ;; `global-clipetty-mode' will not cause issues if enabled for a server with
  ;; both graphical and terminal frames
  (general-after-tty
    (global-clipetty-mode)))

(use-package which-key
  :defer 1
  :hook (elpaca-after-init-hook . which-key-mode)
  ;; replace `where-is'; don't need because can show in M-x
  :init
  ;; should be set before loading
  (setq which-key-idle-delay 0.3)
  :config
  (map ju-toggle-map "W" #'which-key-mode)
  (map help-map "w" #'which-key-show-top-level)
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-prefix-then-key-order
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-sort-uppercase-first nil)
  (which-key-mode))

(use-package ibuffer
  :ensure nil
  :init (require '+ibuffer)
  :config
  (map ju-buffer-map "i" #'ibuffer)
  (map ibuffer-mode-map
       "<tab>" #'ibuffer-toggle-filter-group
       "q" #'kill-this-buffer)
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")
              (ibuffer-update nil t)
              (ibuffer-auto-mode 1)))

  (setq ibuffer-use-header-line t
        ibuffer-expert t
        ibuffer-show-empty-filter-groups nil))

;; small QOL
(defalias #'elisp-mode #'emacs-lisp-mode)

(use-package password-store)
(use-package password-store-otp)
(use-package pass)

;; ----------------------------

(use-package vundo :ensure t)
(use-package undo-fu :ensure t
  :bind (("C-/" . #'undo-fu-only-undo)
         ("C-S-/" . #'undo-fu-only-redo))
  :custom
  (undo-limit 67108864)          ; 64mb.
  (undo-strong-limit 100663296)  ; 96mb.
  (undo-outer-limit 1006632960)) ; 960mb.

;; endup corrupting too frequently
(use-package undo-tree
  :disabled t
  :init
  (csetq-default undo-tree-history-directory-alist
                 `((".*" . ,(concat cache-dir "undo-tree/"))))
  (global-undo-tree-mode 1))

(use-package eshell
  :ensure
  :hook
  (eshell-mode-hook . (lambda () (setq outline-regexp eshell-prompt-regexp)))
  :init
  (require 'em-smart)
  (setq eshell-where-to-jump 'begin)
  (setq eshell-review-quick-commands nil)
  (setq eshell-smart-space-goes-to-end t)
  ;; Stop asking if you really want to open eshell
  (put 'eshell 'disabled nil)
  (csetq eshell-login-script (concat emacs-dir ".eshell_profile"))
  (after! consult
    (require 'em-hist)
    (map eshell-hist-mode-map "C-c C-l" #'consult-history)))

(use-package eat
  :custom
  (eat-enable-shell-prompt-annotation t)
  :bind
  (:map eat-mode-map
        ("C-t" . 'ju-menu-map)
        ("C-l" . #'eat-reset))
  :init
  ;; For `eat-eshell-mode'.
  (add-hook! 'eshell-load-hook #'eat-eshell-visual-command-mode #'eat-eshell-mode))


(provide 'essentials)
;;; essentials.el ends here.
