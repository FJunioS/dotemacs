;;; core-packages.el ---  desc  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'core-lib)
(require 'core-load-paths)

(defvar elpaca-installer-version 0.6)
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
    (csetq package-enable-at-startup nil)
    (package-initialize)
    (pushnew! package-archives
              '("nongnu" . "https://elpa.nongnu.org/nongnu/")
              '("melpa" . "https://melpa.org/packages/"))))

(require 'core-window)
(core-handle-popup (rx "*elpaca-log*"))
;; don't require `use-package' when loading compiled file; saves a millisecond
;; or 2; compiling now saves ~0.1s overall (maybe another 0.1s after general
;; rewrite)

(eval-when-compile
  (require 'use-package)
  ;; don't actually need `eval-when-compile' for rest since currently loading
  ;; entire init file before compiling already
  (csetq use-package-always-defer t))

(defvar +elpaca-hide-log-commands '( eval-buffer eval-region eval-defun +eval-region-or-sexp
                                     eval-last-sexp org-ctrl-c-ctrl-c)
  "List of commands for which a successfully processed log is auto hidden.")
(defun +elpaca-hide-successful-log ()
  "Hide Elpaca log buffer if queues processed successfully."
  (message "this: %S last: %S" this-command last-command)
  (if-let ((incomplete (cl-find 'incomplete elpaca--queues :key #'elpaca-q<-status))
           ((elpaca-q<-elpacas incomplete)))
      nil
    (when-let ((log (bound-and-true-p elpaca-log-buffer))
               (window (get-buffer-window log t)) ;; log buffer visible
               ((or (member last-command +elpaca-hide-log-commands)
                    (member this-command +elpaca-hide-log-commands))))
      (with-selected-window window (quit-window 'kill window)))))

(add-hook 'elpaca-post-queue-hook #'+elpaca-hide-successful-log)

(elpaca-wait)
(add-hook 'elpaca-after-init-hook (lambda () (setq-default eldoc-mode 1)))

(use-package org :elpaca nil)

(use-package blackout :demand t)
(use-package diminish :demand t)
(use-package general
  :demand t
  :ensure t
  :config)

(use-package async
  :demand t
  :ensure t
  :config
  (async-bytecomp-package-mode 1))

(use-package hydra
  :demand t
  :ensure t)

(use-package auth-source
  :no-require t
  :ensure t
  :demand t
  :elpaca nil
  :config (setq auth-sources '("~/.authinfo.gpg" "~/.authinfo" "~/.netrc")))

(use-package exwm
  :demand t
  :if (string= (getenv "XDG_CURRENT_DESKTOP") "EXWM")
  :config
  (exwm-enable)
  (server-start)
  (require '+exwm))

(elpaca-wait)

(use-package desktop-environment
  :ensure t
  :after exwm
  :config
  (desktop-environment-mode)

  :custom
  (desktop-environment-brightness-small-increment "2%+")
  (desktop-environment-brightness-small-decrement "2%-")

  (desktop-environment-brightness-normal-increment "5%+")
  (desktop-environment-brightness-normal-decrement "5%-")

  :config
  (defhydra hydra-volume (:timeout 4)
    "Configure Volume"
    ("j" desktop-environment-volume-normal-increment "up")
    ("k" desktop-environment-volume-normal-decrement "down")
    ("q" nil "quit" :exit t))

  ;; This hydra function allows for control of brightness
  (defhydra hydra-brightness (:timeout 4)
    "Configure Brightness"
    ("j" desktop-environment-brightness-increment "up")
    ("k" desktop-environment-brightness-decrement "down")
    ("q" nil "quit" :exit t))

  (nvmap :prefix "C-c C-s"
    "" '(:ignore t :wk "System")
    "v" #'hydra-volume/body
    "b" #'hydra-brightness/body))

;; Launch apps with completion on point interface
(use-package app-launcher
  :after exwm
  :elpaca (:host github :repo "SebastienWae/app-launcher"))

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
    (org-crypt-use-before-save-magic))

(provide 'core-packages)
;;; core-packages.el ends here
