;;; core-load-paths.el --- Core standard library -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
;;; Global Variables
(require 'core-lib)

(defvar emacs-dir
  user-emacs-directory
  "Path where Emacs loaded (e.g. $HOME/.emacs.d/ or $HOME/.config/emacs).")

(defun expand-user-emacs-file (file)
  "Expand relative FILE to `user-emacs-directory'."
  (expand-file-name file (file-name-directory emacs-dir)))

(defvar user-notes-dir (expand "~/notes/"))

;; TODO: Move to org handler

(defconst IS-MAC      (eq system-type 'darwin))
(defconst IS-LINUX    (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix)))
(defconst IS-WINDOWS  (memq system-type '(cygwin windows-nt ms-dos)))

(defvar user-dir
  (expand-file-name
   (if-let (cfg-dir (getenv-internal "USER_SYNC_DIR"))
       (file-name-as-directory cfg-dir)
     (or (let ((xdg-dir
                (file-name-concat
                 (or (getenv-internal "HOME")
                     "~")
                 "~/.emacs.d/")))
           (if (file-directory-p xdg-dir) xdg-dir))
   "~/.emacs_cfg.d/")))
  "Where your private configuration is placed.

  It will be $USER_SYNC_DIR, ~/sync/emacs/ or ~/.emacs/,
depends on what is found first Must end in a slash.")

(defvar config-dir (expand-file-name
          (let ((xdg-dir
            (or (getenv-internal "XDG_CONFIG_HOME")
          "~/.config")))
      (if (file-directory-p xdg-dir) xdg-dir))))

(defconst core-dir
  (file-name-directory load-file-name))

(defconst sync-dir
   (if-let (cfg-dir (getenv-internal "USER_SYNC_DIR"))
       (file-name-as-directory cfg-dir)
     (or (let ((xdg-dir
                (file-name-concat
                 (or (getenv-internal "HOME")
                     "~")
                 "sync/")))
           (if (file-directory-p xdg-dir) xdg-dir)))))

(defvar cache-dir
  (expand-file-name "emacs/" (or (getenv-internal "XDG_CACHE_HOME") "~/.cache")))

(defvar tmp-dir
  (if IS-WINDOWS
      (expand-file-name "emacs/tmp/" (getenv-internal "TEMP"))
    (expand-file-name "/tmp/emacs/")))

(make-dir! cache-dir)
(make-dir! tmp-dir)

(provide 'core-load-paths)

;;; core-load-paths.el ends here
