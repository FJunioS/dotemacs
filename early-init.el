;;; early-init.el --- Personal configuration file -*- lexical-binding: t -*-

;; Copyright (c) 2021-2023  Junio Santos <info@junio.dev>

;; Author: Junio Santos <info@junio.dev>
;; URL: https://junio.dev/dotemacs
;; Version: 0.2.0
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of

;; GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is loaded even when Emacs is started in debug mode
;; making essential settings usable when debugging and also
;; can be modified to make startup faster (or slower)
;; and all settings defined here can be used for the rest of application
;; Have fun ;)
;;
;;; Code:
(require 'cl-lib)

(add-to-list 'load-path (locate-user-emacs-file "core"))

(require 'core-window)
(core-handle-popup (rx "*Messages*"))
(core-handle-popup (rx "*Warnings*"))
(core-handle-popup (rx "*Backtrace*"))

;; Avoid wasting time loading before init files, also as we are
;; going to use Straight this would only delay our startup
(setq package-enable-at-startup nil)

;; Temporarily increase the garbage collection threshold.  These
;; changes help shave off about half a second of startup time.
;; later on we'll fix this with `gchm.el'
(setq gc-cons-threshold most-positive-fixnum)

(setq load-prefer-newer noninteractive)

;; PERF: A second, case-insensitive pass over `auto-mode-alist' is time wasted.
(setq auto-mode-case-fold nil)

(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(alpha . (80 . 80))    default-frame-alist)

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

;; Don't ping things that look like domain names.
;; Faster redisplay
(setq bidi-inhibit-bpa t)  ; Emacs 27+

(setq ffap-machine-p-known 'reject)

;; Emacs "updates" its ui more often than it needs to, so slow it down slightly
(setq idle-update-delay 1.0) ; Default: 0.5
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

;; Let Version Control handle backups
(setq create-lockfiles nil
      make-backup-files nil
      ;; But in case the user does enable it, some sensible defaults:
      version-control t     ; number each backup file
      backup-by-copying t   ; instead of renaming current file (clobbers links)
      delete-old-versions t ; clean up after itself
      kept-old-versions 5
      kept-new-versions 5
      tramp-backup-directory-alist backup-directory-alist)

;; Better scroll performance by inhibiting fontification while receiving input
(setq redisplay-skip-fontification-on-input t)
(setq fast-but-imprecise-scrolling t)

(setq site-run-file nil
      inhibit-default-init t)
(set-face-attribute 'default nil
                    :background "#000" :foreground "#fff")
(set-face-attribute 'mode-line nil
                    :background "#000" :foreground "#fff")

;; By blocking resize events until later, we avoid expensive screen redrawing
(setq frame-inhibit-implied-resize t)

;; (advice-add #'display-startup-echo-area-message :override #'ignore)
;; (advice-add #'display-startup-screen :override #'ignore)

(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

(add-hook 'after-init-hook
          (lambda () (set-frame-name "home")))

(unless (and (daemonp) noninteractive)
  (unless initial-window-system
    ;; PERF: Inexplicably, `tty-run-terminal-initialization' can sometimes
    ;;   take 2-3s when starting up Emacs in the terminal. Whatever slows it
    ;;   down at startup doesn't appear to affect it if it's called a little
    ;;   later in the startup process, so that's what I do.
    ;; REVIEW: This optimization is not well understood. Investigate it!
    (define-advice tty-run-terminal-initialization (:override (&rest _) defer)
      (advice-remove #'tty-run-terminal-initialization
                     #'tty-run-terminal-initialization@defer))))

;;   (unless init-file-debug
;;     ;; (define-advice load-file (:override (file) silence)
;;     ;;   (load file nil 'nomessage))
;;
;;     (put 'mode-line-format 'initial-value (default-toplevel-value 'mode-line-format))
;;     (setq-default mode-line-format nil)
;;
;;     (dolist (buf (buffer-list))
;;       (with-current-buffer buf (setq mode-line-format nil)))
;;
;;     (setq-default inhibit-redisplay t
;;                   inhibit-message t)
;;
;;     ;; COMPAT: Then reset it with advice, because `startup--load-user-init-file'
;;     ;;   will never be interrupted by errors. And if these settings are left
;;     ;;   set, Emacs could appear frozen or garbled.
;;     (defun doom--reset-inhibited-vars-h ()
;;       (setq-default inhibit-redisplay nil
;;                     ;; Inhibiting `message' only prevents redraws and
;;                     inhibit-message nil)
;;       (redraw-frame))
;;
;;     (add-hook 'after-init-hook #'doom--reset-inhibited-vars-h)
;;     (define-advice startup--load-user-init-file (:after (&rest _) undo-inhibit-vars)
;;       (when init-file-had-error
;;         (doom--reset-inhibited-vars-h))
;;       (unless (default-toplevel-value 'mode-line-format)
;;         (setq-default mode-line-format (get 'mode-line-format 'initial-value))))
;;
;;     (advice-add #'tool-bar-setup :override #'ignore)
;;     (define-advice startup--load-user-init-file (:before (&rest _) defer-tool-bar-setup)
;;       (advice-remove #'tool-bar-setup #'ignore)
;;       (add-transient-hook! 'tool-bar-mode (tool-bar-setup)))))
;;
;; (load (expand-file-name "init.el" (file-name-directory load-file-name)) nil 'nomessage)
