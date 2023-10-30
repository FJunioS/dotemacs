;;; core-packages.el ---  desc  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

;; ** `'Straight.el' :: Package Manager
(eval-and-compile
  (setq straight-enable-use-package-integration t
        ;; check for modifications (to determine whether a package needs to be
        ;; rebuilt) using `after-save-hook' instead of during startup or always
        ;; rebuilding packages (https://github.com/raxod502/straight.el/issues/41)
        ;; drops loading straight from ~0.88s to ~0.05s; needs to be set
        ;; before loading straight
        straight-check-for-modifications '(check-on-save find-when-checking)
        ;; install packages by default (like use-package's `use-package-always-ensure')
        straight-use-package-by-default t
        ;; store all autoloads in one file; default t
        straight-cache-autoloads t))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defvar core-straight-packages nil
  "List of packages `straight-use-package' is successful for.")

(defun core-straight-use-package (fn &rest args)
  "Store a package (FN ARGS) in `core-straight-packages' on success."
  (when (apply fn args)
    (push (if (listp (car args))
              (caar args)
            (car args))
          core-straight-packages)))
(advice-add 'straight-use-package :around #'core-straight-use-package)
;; Prevent "org version mismatch" error
(straight-use-package 'org)

;;; ** Use-package
;; need to do this early again to prevent "org version mismatch" error
(straight-use-package 'use-package)

;; don't require `use-package' when loading compiled file; saves a millisecond
;; or 2; compiling now saves ~0.1s overall (maybe another 0.1s after general
;; rewrite)
(eval-when-compile
  (require 'use-package)

  ;; don't actually need `eval-when-compile' for rest since currently loading
  ;; entire init file before compiling already
  (setq use-package-always-defer t)

  (defun core-package-available-p (package keywords)
    "Return whether PACKAGE is in `core-straight-packages'.
    Also return non-nil if KEYWORDS contains :straight nil."
    (or (and (memq :straight keywords)
             (null (plist-get keywords :straight)))
        ;; must be quoted so doesn't check at expansion time
        `(memq ',package core-straight-packages)))

  ;; don't do anything if installation fails; like
  ;; `use-package-check-before-init' but works for :config and other keywords;
  ;; recording `straight-use-package' return values instead of using

  (cl-pushnew '(:when
                core-package-available-p
                t)
              use-package-defaults
              :test #'equal))

;; demote installation errors to messages
;; this variable is no longer changed by straight
;; (advice-add use-package-ensure-function :around #'noct-use-package-ensure)
(when (bound-and-true-p core-with-demoted-errors)
  (advice-add 'straight-use-package :around #'core-inhibit-error-advice))

(straight-use-package 'blackout)

(defvar leader-key "SPC")
(defvar non-normal-leader-key "C-c")
(use-package general
  :defer 0
  :ensure t
  :config
  (general-auto-unbind-keys)
  (general-create-definer leader
    :states '(normal visual emacs)
    :prefix leader-key
    :non-normal-prefix non-normal-leader-key)

  (defalias 'def #'general-def)
  (defalias 'kbd! #'general-simulate-key)
  (defalias 'gsetq #'general-setq)
  (defalias 'gsetq-default #'general-setq-default)
  (defalias 'gsetq-local #'general-setq-local)
  (require '+general))

(provide 'core-packages)
;;; core-packages.el ends here
