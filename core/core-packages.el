;; -*- lexical-binding: t; -*-
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

(defvar +elpaca-hide-log-commands '(eval-buffer eval-region eval-defun +eval-region-or-sexp
                                                eval-last-sexp org-ctrl-c-ctrl-c pp-eval-last-sexp pp-eval-expression)
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

(defun +elpaca-reload-package (package &optional allp)
  "Reload PACKAGE's features.
If ALLP is non-nil (interactively, with prefix), load all of its
features; otherwise only load ones that were already loaded.

This is useful to reload a package after upgrading it.  Since a
package may provide multiple features, to reload it properly
would require either restarting Emacs or manually unloading and
reloading each loaded feature.  This automates that process.

Note that this unloads all of the package's symbols before
reloading.  Any data stored in those symbols will be lost, so if
the package would normally save that data, e.g. when a mode is
deactivated or when Emacs exits, the user should do so before
using this command."
  (interactive
   (list (let ((elpaca-overriding-prompt "Reload package: "))
           (elpaca--read-queued))
         current-prefix-arg))
  ;; This finds features in the currently installed version of PACKAGE, so if
  ;; it provided other features in an older version, those are not unloaded.
  (when (yes-or-no-p (format "Unload all of %s's symbols and reload its features? " package))
    (let* ((package-name (symbol-name package))
           (package-dir (file-name-directory
                         (locate-file package-name load-path (get-load-suffixes))))
           (package-files (directory-files package-dir 'full (rx ".el" eos)))
           (package-features
            (cl-loop for file in package-files
                     when (with-temp-buffer
                            (insert-file-contents file)
                            (when (re-search-forward (rx bol "(provide" (1+ space)) nil t)
                              (goto-char (match-beginning 0))
                              (cadadr (read (current-buffer)))))
                     collect it)))
      (unless allp
        (setf package-features (seq-intersection package-features features)))
      (dolist (feature package-features)
        (ignore-errors
          ;; Ignore error in case it's not loaded.
          (unload-feature feature 'force)))
      (dolist (feature package-features)
        (require feature))
      (when package-features
        (message "Reloaded: %s" (mapconcat #'symbol-name package-features " "))))))
(add-to-list 'elpaca-ignored-dependencies 'eldoc)

(add-hook 'elpaca-post-queue-hook #'+elpaca-hide-successful-log)

(elpaca-wait)
(use-package org)
;; Silence modes from displaying on mode-linep
(use-package diminish :demand t)
(use-package async
  :demand t
  :ensure t
  :config
  (async-bytecomp-package-mode 1))
(use-package with-editor)
(use-package transient)
(use-package hydra
  :demand t
  :ensure t)
(use-package popup)
(use-package dash)
(use-package s)
(use-package f)
(use-package auth-source
  :ensure
  :config
  (setq auth-sources '("~/.authinfo.gpg" "~/.netrc"))
  (auth-source-pass-enable))

(use-package pinentry
  :ensure t
  :init
  ;; let's get encryption established
  (setenv "GPG_AGENT_INFO" nil)  ;; use emacs
  (setenv "SSH_AUTH_SOCK" (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket"))

  (setq auth-source-debug t)

  (require 'epa-file)
  (epa-file-enable)
  (setq epa-pinentry-mode 'loopback)
  (setq epg-pinentry-mode 'loopback)
  (pinentry-start t)

  (require 'org-crypt)
  (org-crypt-use-before-save-magic)

  ;; Start GPG agent with SSH support
  (shell-command "gpg-connect-agent /bye"))

(elpaca-wait)

(provide 'core-packages)
;;; core-packages.el ends here.
