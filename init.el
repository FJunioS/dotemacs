;; -*- lexical-binding: t; -*-
;; init.el -*- lexical-binding: t -*-

;; Copyright (c) 2021-2023  Junio Santos <info@junio.dev>

;; Author: Junio Santos <hello@junio.dev>
;; URL: https://junio.dev/dotemacs
;; Version: 0.2.0
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hoke that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, seek<https://www.gnu.org/licenses/>.

;;; Code:

;;; Setup path
(require 'subr-x)
(require 'cl-lib)

(defvar emacs-dir (if (string= (expand-file-name "~/.cache/emacs/") user-emacs-directory)
                      (if (file-exists-p "~/.emacs.d/")
                          (expand-file-name "~/.emacs.d/")
                        (expand-file-name "~/.config/emacs/"))
                    ;; else
                    (expand-file-name user-emacs-directory))
  "Emacs root directory.")

(add-to-list 'load-path  (concat emacs-dir "/core/"))
(add-to-list 'load-path (concat emacs-dir "layers/"))
(add-to-list 'load-path (concat emacs-dir "lisp/"))

(require 'core-lib)
(require 'core-load-paths)

;;; Early optimizations
(defun backup-predicate (file)
  "Return whether to backup FILE.
Don't backup remote directories or encrypted files."
  (not (or (file-remote-p file)
           (string-match-p (car epa-file-handler) file))))
;;(when (interactivep)
(setq frame-resize-pixelwise t
	    frame-inhibit-implied-resize t
	    use-dialog-box nil ; only for mouse events, which I seldom use
	    use-file-dialog nil
	    inhibit-splash-screen t
	    inhibit-startup-screen t
	    inhibit-x-resources t
	    inhibit-startup-echo-area-message user-login-name ; read the docstring
	    inhibit-startup-buffer-menu t)
;;)

(defadvice! __shut-up-autosave-a (fn &rest args)
  "If a file has autosaved data, `after-find-file' will pause for 1 second to
tell you about it. Very annoying. This prevents that."
  :around #'after-find-file
  (letf! ((#'sit-for #'ignore))
    (apply fn args)))
;; ** User info
(setq-default user-full-name "Junio Santos"
              user-mail-address "git@junio.dev")
;; Avoid buffer lag
(setq process-adaptive-read-buffering nil)
(setq read-process-output-max (* 4 1024 1024)) ; from 800Kb to 4Mb

;; Redirect autosave files to `cache-dir'
(setq backup-directory-alist (list (cons "." (concat cache-dir "backup/")))
      auto-save-list-file-prefix (concat cache-dir "autosave/")
      tramp-auto-save-directory  (concat cache-dir "tramp-autosave/"))

;; Bookmarks
(setq bookmark-default-file (concat cache-dir "bookmarks"))

(defun init--enable-messages-buffer-after-init ()
  "Enable messaging after init and it doesn't populate message
  buffer with loading notification"
  (run-with-timer
   1 nil (lambda () (setq inhibit-message nil))))

(put 'set-goal-column 'disabled nil)

(setq ad-redefinition-action 'accept)

(setq warning-suppress-types '((defvaralias)))

(setq debug-on-error init-file-debug
      jka-compr-verbose init-file-debug)

(setq-default abbrev-mode t)
(setq case-fold-search t) ; Case insensitive searches
(setq font-lock-maximum-decoration t)
(setq global-auto-revert-non-file-buffers t)

(setq set-mark-command-repeat-pop t)
;; stop describe-function from converting many quote characters in docstring
(setq text-quoting-style 'grave)

;; Linux copying is transferred to Emacs kill-ring
;; (setq select-enable-clipboard t)
;; (setq select-enable-primary t)  ; Mouse selection yanks
;; Basic modes
(electric-pair-mode 1)
(global-subword-mode 1)
(global-auto-revert-mode 1)
(winner-mode 1)
(repeat-mode 1)
(global-goto-address-mode 1)
(delete-selection-mode 1)
(global-visual-line-mode 1)
(minibuffer-depth-indicate-mode 1)
(blink-cursor-mode -1)
(undelete-frame-mode 1) ; Emacs 29
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)
(add-hook 'org-mode-hook  #'hl-line-mode)
(setq find-file-visit-truename t
      vc-follow-symlinks t)
(defun NOP-kill-new (orig-func &rest args)
  "Run ORIG-FUNC with ARGS preventing any `kill-new's from running."
  ;; http://endlessparentheses.com/understanding-letf-and-how-it-replaces-flet.html
  (cl-letf (((symbol-function 'kill-new) #'ignore))
    (apply orig-func args)))
(advice-add 'backward-kill-word :around #'NOP-kill-new)
(advice-add 'kill-word :around #'NOP-kill-new)

(setq enable-recursive-minibuffers t)

(setq ring-bell-function 'ignore)

(setq-default fill-column 80)
(setq mouse-autoselect-window t
      focus-follows-mouse t)
(csetq visual-line-fringe-indicators '(nil nil))
(csetq split-height-threshold 2
       split-width-threshold 2)

(setq scroll-margin 5
      scroll-conservatively 1)

(setq uniquify-buffer-name-style 'forward)
(setq sentence-end-double-space t)

(setq-default confirm-kill-processes nil) ;emacs 26

;; https://endlessparentheses.com/faster-pop-to-mark-command.html
(defadvice pop-to-mark-command
    (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point))
        ad-do-it))))

(setq-default indent-tabs-mode nil
              tab-width 4)
(setq-hook! 'emacs-lisp-mode-hook tab-width 2)

(setq message-log-max 10000)
(setq kill-ring-max 300)
(setq history-length 3000
      history-delete-duplicates t)
(setq frame-title-format '("%b"))
(setq use-short-answers t)
(setq native-compile-prune-cache t) ; Emacs 29
(setq native-comp-async-report-warnings-errors 'silent)
(setq custom-file (expand-file-name "custom.el" cache-dir))

;; increase number of messages
(setq fast-but-imprecise-scrolling t)
(setq adaptive-fill-mode nil)
(setq kill-do-not-save-duplicates t)
(setq show-paren-style 'mixed)

;; Emacs by default will warn you when you use some commands for the first time.
(dolist (c '(narrow-to-region narrow-to-page upcase-region downcase-region
	           eshell erase-buffer scroll-left dired-find-alternate-file))
  (put c 'disabled nil))

;; And disable these
(dolist (c '(overwrite-mode iconify-frame diary))
  (put c 'disabled t))

(setq x-gtk-use-system-tooltips nil
      pos-tip-internal-border-width 1)
(setq x-underline-at-descent-line t)
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(symbol-function 'xah-select-line)
(setq idle-update-delay 1)
(defun my/restore-update-delay ()
  (setq idle-update-delay 0.5))
(add-hook 'after-init-hook 'my/restore-update-delay)

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

(setq debug-on-error t)
(setq inhibit-message t)
(require 'core-compilation nil t)
(require 'core-packages nil t)

;;; Allow non-floating resizing with mouse.
(csetq window-divider-default-bottom-width 2
       window-divider-default-right-width 2)
(window-divider-mode)

;; Avoid packages store cache on emacs folder.
(setq user-emacs-directory cache-dir)
(defconst ju-layers
  '(keymaps
    essentials
    ;; email
    OS
    window
    navigation
    text-editing
    completion
    dir
    langs
    ui
    git
    notes
    readers)
  "list of layers to load.")
(dolist (mod ju-layers)
  (require mod nil t))
(setq debug-on-error init-file-debug)
(setq inhibit-message nil)

(let ((cfile (concat emacs-dir "custom.el")))
  (when (file-exists-p cfile)
    (setq custom-file cfile))
  (add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror))))

(require 'server)
(unless (server-running-p)
  (server-start))

;;END OF INIT FILE
;; prevents `elpaca-after-init-hook` from running more than once.
(when (require 'elpaca nil t)
  (setq elpaca-after-init-time (or elpaca-after-init-time (current-time)))
  (elpaca-wait))

;;; init.el ends here.
