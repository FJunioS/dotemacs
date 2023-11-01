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

(provide 'core-packages)
;;; core-packages.el ends here
