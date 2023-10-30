;; init.el --- Personal configuration file -*- lexical-binding: t -*-

;; Copyright (c) 2021-2023  Junio Santos <info@junio.dev>

;; Author: Junio Santos <info@junio.dev>
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

;;; Commentary:

;; Currently I've been using some configuration as reference
;; from Sacha Chu(i) and Protesilaos stravou(ii).  Thank you very much
;; (i): https://sachachua.com/
;; (ii): https://git.sr.ht/~protesilaks/dotfiles

;; See my dotfiles: <https://junio.dek/dotfiles>
;;;; Code:

;;; Setup path
(require 'subr-x)
(require 'cl-lib)

(dolist (path '("core" "core/libs" "layers"))
  (add-to-list 'load-path (locate-user-emacs-file path)))

(defgroup ju nil
  "group of custom settings"
  :group 'convenience
  :prefix "ju-")

(defgroup ju-core nil
  "group of custom settings"
  :group 'ju
  :prefix "ju-core-")

(define-widget 'ju-alist 'lazy
  "Ju's alist type."
  :type '(alist :key-type (or symbol (repeat symbol))
                :value-type symbol))

(defcustom ju-core-use-evil-mode
  :group 'ju-core
  :type 'boolean)

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

;; Redirect autosave files to `cache-dir'
(setq backup-directory-alist (list (cons "." (concat cache-dir "backup/")))
      auto-save-list-file-prefix (concat cache-dir "autosave/")
      tramp-auto-save-directory  (concat cache-dir "tramp-autosave/"))

(defun init--enable-messages-buffer-after-init ()
  "Enable messaging after init and it doesn't populate message
  buffer with loading notification"
  (run-with-timer
   1 nil
   (lambda ()
     (setq inhibit-message nil))))

(put 'set-goal-column 'disabled nil)

(setq ad-redefinition-action 'accept)

(setq warning-suppress-types '((defvaralias)))

(setq debug-on-error init-file-debug
      jka-compr-verbose init-file-debug)

(setq-default abbrev-mode t)
(setq case-fold-search t) ; Make searches case insensitive
(setq font-lock-maximum-decoration t)
(setq global-auto-revert-non-file-buffers t)

(delete-selection-mode) ; Delete selection when doing any action (e.g. paste)
(global-auto-revert-mode)

(repeat-mode)
(setq set-mark-command-repeat-pop t)
;; stop describe-function from converting many quote characters in docstring
(setq text-quoting-style 'grave)

;; Linux copying is transferred to Emacs kill-ring
(setq select-enable-clipboard t)
(setq select-enable-primary t)  ; Mouse selection yanks
(electric-pair-mode 1)
(global-subword-mode)
(add-hook! '(prog-mode-hook org-mode-hook text-mode-hook) #'hl-line-mode)

(setq find-file-visit-truename t
      vc-follow-symlinks t)

(defun NOP-kill-new (orig-func &rest args)
  "Run ORIG-FUNC with ARGS preventing any `kill-new's from running."
  ;; http://endlessparentheses.com/understanding-letf-and-how-it-replaces-flet.html
  (cl-letf (((symbol-function 'kill-new) #'ignore))
    (apply orig-func args)))

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode)
(blink-cursor-mode -1)

(setq-default fill-column 80)

(global-visual-line-mode 1)
(setq visual-line-fringe-indicators '(t t))
(setq split-height-threshold nil
      split-width-threshold 0)

(setq scroll-margin 5
      scroll-conservatively 20)

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

(auto-save-visited-mode)
(setq kill-ring-max 300)
(setq history-length 3000
      history-delete-duplicates t)
(setq frame-title-format '("%b"))
(setq ring-bell-function 'ignore)
(setq use-short-answers t)
(setq native-compile-prune-cache t) ; Emacs 29
(setq native-comp-async-report-warnings-errors 'silent)
(setq custom-file (expand-file-name "custom.el" emacs-dir))

;; increase number of messages
(setq message-log-max 10000)
(setq fast-but-imprecise-scrolling t)
(setq kill-do-not-save-duplicates t)
(setq adaptive-fill-mode nil)
(setq show-paren-style 'mixed)
(undelete-frame-mode) ; Emacs 29


(setq-default indent-tabs-mode nil
              tab-width 4)
(setq-hook! 'emacs-lisp-mode-hook
  tab-width 2)

;; Emacs by default will warn you when you use some commands for the first time.
(dolist (c '(narrow-to-region narrow-to-page upcase-region downcase-region
	           erase-buffer scroll-left dired-find-alternate-file))
  (put c 'disabled nil))

;; And disable these
(dolist (c '(eshell project-eshell overwrite-mode iconify-frame diary))
  (put c 'disabled t))

(setq x-gtk-use-system-tooltips nil
      pos-tip-internal-border-width 1)
(setq x-underline-at-descent-line t)
(setq require-final-newline t)
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)
(setq idle-update-delay 1)

(setq debug-on-error t)
(setq inhibit-message nil)
(require 'core-compilation nil t)
(require 'core-packages nil t)

(defconst layers
  '(essentials
    window
    navigation
    text-editing
    completion
    dir
    langs)
  "List of layers to load.")
(dolist (mod layers)
  (require mod nil t))

(defconst interactive-layers '(ui git notes readers)
  "List of common layers that can be loaded with daemon load.")
(dolist (mod interactive-layers)
  (require mod nil t))

(setq debug-on-error init-file-debug)
(setq inhibit-message (not init-file-debug))

(require 'server)
(unless (server-running-p)
(server-start))

;;; init.el ends here
