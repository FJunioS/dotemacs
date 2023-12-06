;;; essentials.el --- Variables and main user settings  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'cl-lib)
(require 'popup-handler)
(require 'core-packages)

(display-time-mode)
(csetq display-time-24hr-format t
      display-time-day-and-date t
      display-time-load-average-threshold nil)

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

(use-package savehist
  :elpaca nil
  :demand t
  :ensure t
  :defer 0
  :init
  (savehist-mode 1)
  (auto-save-mode 1)
  (save-place-mode 1)
  :config
  (csetq-default
   savehist-file (concat cache-dir "savehist-backup")
   save-place-file (concat cache-dir "saveplace"))
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
   auto-save-include-big-deletions t
   auto-save-list-file-prefix (expand-file-name "autosave/" cache-dir)
   auto-save-file-name-transforms `((".*" ,auto-save-list-file-prefix t)))

  (add-hook 'kill-emacs-hook #'savehist-save)
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
                     else collect (cons reg item))))))

(general-with-package 'compile
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
  (noct-handle-popup compilation-mode))

(require 'zone-words)

(use-package vimish-fold
  :config
  (vimish-fold-global-mode))

(use-package apropos
  :elpaca nil
  :init
  ;; TODO can this be put in :config?
  (noct-handle-popup apropos-mode)
  :config
  (setq apropos-do-all t)
  (define-key apropos-mode-map "q" #'quit-window))

(use-package helpful
  :general
  (general-def help-map
    "RET" #'helpful-at-point
    "f" #'helpful-callable
    "v" #'helpful-variable
    "k" #'helpful-key
    "o" #'helpful-symbol)
  (general-def helpful-mode
    :definer 'minor-mode
    "q" #'quit-window)
  :init
  ;; using this instead of binding them directly allows taking an alternate action
  ;; without also opening the helpful buffer
  (setq counsel-describe-function-function #'helpful-callable
        counsel-describe-variable-function #'helpful-variable)
  :config
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
  :ghook ('pre-command-hook nil nil nil t)
  :config
  ;; settings used by doom; default infinite threshold causes Emacs to
  ;; completely freeze after working for a while; maybe this will be better
  (setq gcmh-idle-delay 10
        gcmh-high-cons-threshold 16777216)
  (general-add-hook 'focus-out-hook #'gcmh-idle-garbage-collect))

(use-package recentf
  :elpaca nil
  :ghook 'elpaca-after-init-hook
  :init (recentf-mode)
  :custom
  (recentf-save-file (concat cache-dir "recentf"))
  :general
  ("C-x C-r" #'recentf)
  (leader/file
    "r" #'recentf)
  :config
  (setq recentf-auto-cleanup nil
        recentf-max-saved-items 200)
  (setq recentf-exclude '("^/tmp/emacs/.*" "^/var/folders\\.*" "COMMIT_EDITMSG\\'"
                          ".*-autoloads\\.el\\'" "[/\\]\\.elpa/"))

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
  :ghook ('pre-command-hook nil nil nil t)
  :general
  (leader/toggle "W" #'which-key-mode)
  ;; replace `where-is'; don't need because can show in M-x
  (help-map "w" #'which-key-show-top-level)
  :init
  ;; should be set before loading
  (setq which-key-idle-delay 0.3)
  :config
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-prefix-then-key-order
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-sort-uppercase-first nil)
  (which-key-mode))

(use-package ibuffer
  :elpaca nil
  :init (require '+ibuffer)
  :general
  (leader/buffer "i" #'ibuffer)
  (general-def 'ibuffer-mode-map
   "<tab>" #'ibuffer-toggle-filter-group
   "q" #'kill-this-buffer)
  :config
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (ibuffer-switch-to-saved-filter-groups "default")
              (ibuffer-update nil t)
              (ibuffer-auto-mode 1)))

  (setq ibuffer-use-header-line t
        ibuffer-expert t
        ibuffer-show-empty-filter-groups nil))

(use-package vundo
  :config
  (csetq vundo-glyph-alist vundo-unicode-symbols)
  (set-face-attribute 'vundo-default nil :family "Symbola"))

(use-package eat)

;; Even coming together with Magit, it is nice to have this separately.
(use-package transient)

;;;###autoload
(defun ju-restart ()
  (interactive)
  (start-process "emacsclient" nil "pkill"
                 "-KILL" "emacs" "&&" "emacsclient" "-ca"))


(provide 'essentials)
;;; essentials.el ends here
