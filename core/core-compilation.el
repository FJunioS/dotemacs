;;; core-compilation.el --- _ -*- lexical-binding: t -*-
;;; Commentary:
;; Thanks for Spacemacs by disponilizing this solution
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'bytecomp)

(defvar e--last-emacs-version ""
  "This variable is set during Emacs initialization to its version.")
(defconst e--last-emacs-version-file
  (expand-file-name (concat cache-dir "last-emacs-version"))
  "File that sets `--last-emacs-version' variable.")

(cl-defun e//dir-byte-compile-state (dir &optional update)
  "Get the directory byte-compile state.
When the UPDATE is t, it will force update the state."
  (let ((state (gethash dir e--dir-byte-compile-status)))
    (when (and (not update) state)
      (cl-return-from e//dir-byte-compile-state state))
    (setq state nil)
    (remhash dir e--dir-byte-compile-status)
    (let ((afiles '())
          (slist (mapcan
                  (lambda (x)
                    (mapcar (lambda (y) (concat x y)) load-file-rep-suffixes))
                  (list ".el" (byte-compile-dest-file ".el")))))
      (cl-dolist (file (directory-files-recursively dir "\\.elc?\\(\\.gz\\)?$"))
        (let* ((name (file-name-sans-extension file))
               (paths (alist-get name afiles nil nil 'equal)))
          (unless paths
            (setq paths (list nil nil nil nil))
            (push (cons name paths) afiles))
          (if-let ((idx (cl-loop for i from 0
                                 for s in slist
                                 until (string-suffix-p s file)
                                 finally return i)))
              (setf (nth idx paths) file))))
      (cl-dolist (item (mapcar 'cdr afiles))
        (let ((el (or (nth 0 item) (nth 1 item)))   ; .el or .el.gz file
              (elc (or (nth 2 item) (nth 3 item)))) ; .elc or .elc.gz file
          (pcase nil
            ((guard (null el))            ; *.el not exists
             (puthash dir -1 e--dir-byte-compile-status)
             (cl-return-from e//dir-byte-compile-state -1))
            ((guard (null elc))           ; *.elc not exists
             (when (null state)
               (setq state 0)))
            ((guard (file-newer-than-file-p el elc)) ; *.elc is older
             (puthash dir -1 e--dir-byte-compile-status)
             (cl-return-from e//dir-byte-compile-state -1))
            (_
             (setq state 1)))))
      (puthash dir state e--dir-byte-compile-status)
      state)))

(defun e//remove-byte-compiled-files-in-dir (dir)
  "Remove all .elc files in DIR directory."
  (dolist (elc (directory-files-recursively dir "\\.elc\\(\\.gz\\)?$"))
    (when (file-exists-p elc)
      (delete-file elc))))

(defvar e--dir-byte-compile-status
  (make-hash-table :test 'equal)
  "The hash table to store each directory byte compile state.
nil for un-initialized, -1 for stale or orphaned *.elc,
0 for no *.elc, 1 for *.elc corresponding to *.el.")

(defun e//update-last-emacs-version ()
  "Update `e--last-emacs-version' and its saved value."
  (with-temp-file e--last-emacs-version-file
    (insert (format "(setq --last-emacs-version %S)"
                    (setq e--last-emacs-version emacs-version)))
    (make-directory (file-name-directory e--last-emacs-version-file)
                    t)))


(defun emacs-version-synced-p ()
  (string= e--last-emacs-version emacs-version))

(defun e--became-stale-p ()
  (defun e--check-eln-dir ()
    (e//dir-byte-compile-state
     (concat cache-dir "eln/")))
  (> 0 (if (not (e--check-eln-dir))
                   0
         (e--check-eln-dir))))

(when (and (featurep 'native-compile)
     (not (native-comp-available-p)))
        (delq 'native-compile features))

;; Native compilation support (see http://akrl.sdf.org/gccemacs.html)
(when (boundp 'native-comp-eln-load-path)
  ;; REVIEW Use `startup-redirect-eln-cache' when 28 support is dropped
  (add-to-list 'native-comp-eln-load-path
               (expand-file-name "eln/" cache-dir))

  ;; UX: Suppress compiler warnings and don't inundate users with their popups.
  ;;   They are rarely more than warnings, so are safe to ignore.
  (setq native-comp-async-report-warnings-errors init-file-debug
        native-comp-warning-on-missing-source init-file-debug)

  (unless (boundp 'native-comp-deferred-compilation-deny-list)
    (defvaralias 'native-comp-deferred-compilation-deny-list
      'native-comp-jit-compilation-deny-list))

  ;; UX: By default, native-comp uses 100% of half your cores. If you're
  ;;   expecting this this should be no issue, but the sudden (and silent) spike
  ;;   of CPU and memory utilization can alarm folks, overheat laptops, or
  ;;   overwhelm less performant systems.
  (define-advice comp-effective-async-max-jobs (:before (&rest _) set-default-cpus)
    "Default to 1/4 of cores in interactive sessions and all of them otherwise."
    (and (null comp-num-cpus)
         (zerop native-comp-async-jobs-number)
         (setq comp-num-cpus
               (max 1 (/ (num-processors) (if noninteractive 1 4))))))

  (define-advice comp-run-async-workers (:around (fn &rest args) dont-litter-tmpdir)
    "Normally, native-comp writes a ton to /tmp. This advice forces it to write
to `doom-cache-dir'/comp/ instead, so that Doom can safely clean it up as part
of 'doom sync' or 'doom gc'."
    (let ((temporary-file-directory (expand-file-name "comp/" cache-dir)))
      (make-directory temporary-file-directory t)
      (apply fn args))))

;; Remove compiled core files if they become stale or Emacs version has changed.
(load e--last-emacs-version-file t (not init-file-debug))
(when (or (not (string= e--last-emacs-version emacs-version))
          (e--became-stale-p))
  (e//remove-byte-compiled-files-in-dir emacs-dir))
;; Update saved Emacs version.
(unless (emacs-version-synced-p)
  (e//update-last-emacs-version))

(provide 'core-compilation)
