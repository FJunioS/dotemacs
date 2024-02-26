;; -*- lexical-binding: t; -*-

(require 'core-lib)
(require 'core-load-paths)
(require 'org)

(defvar loaded-ok nil)

(defun load-config (&optional stable-only)
  "Check config files available and load the most complete.
STABLE-ONLY means that won't load config.el"
  (interactive)

  (if (file-exists-p config-stable-file)
      (load! config-stable-base-file)
    (unless (bound-and-true-p stable-only)
      (load! config-base-file))))

(defun tangle-config-and-load ()
  (interactive)

  (condition-case err
      (progn
        (org-babel-tangle-file config-org-file)
        (load config-tangled-file nil t)
        (setq loaded-ok t))
    ;; Load stable file (for debug) or file with errors to load what it can
    (progn
      (load-config)
      (error err)))

  (when (bound-and-true-p loaded-ok)
    (delete-file config-stable-file t)
    (copy-file config-tangled-file config-stable-file)
    (silently! (byte-compile-file config-stable-file))
    (load-config t)))

(provide 'core-tangle)
;;; core-tangle.el ends here.
