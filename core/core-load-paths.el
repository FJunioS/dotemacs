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

;; (defconst config-base-file        (expand-user-emacs-file "config"))
;; (defconst config-org-file         (concat config-base-file ".org"))
;; (defconst config-tangled-file     (concat config-base-file ".el"))
;; (defconst config-stable-base-file (concat config-base-file "-stable"))
;; (defconst config-stable-file      (concat config-stable-base-file ".el"))

(defvar user-notes-dir (expand "~/sync/notes/"))

;; TODO: Move to org handler
;; (defvar org-directory (expand user-notes-dir))
;; (defvar org-notes (expand user-notes-dir))

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
                 "sync/emacs/")))
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

(defconst emacs-sync-dir
          (file-name-concat sync-dir "emacs/"))

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
