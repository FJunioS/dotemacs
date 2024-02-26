;; -*- lexical-binding: t; -*-

(require 'core-packages)

(use-package dired
  :ensure nil
  :config
  (defun dired--recentf-fix (orig-fun &rest args)
    (let ((recentf-exclude '(".*")))
      (apply orig-fun args)))
  (advice-add 'dired-up-directory :around
              #'dired--recentf-fix))

(with-eval-after-load 'async
  (autoload 'dired-async-mode "dired-async.el" nil t)
  (dired-async-mode 1))

(use-package dired-rsync
  :if (executable-find "rsync")
  :defer t)

(use-package dirvish
  :bind (("C-x C-j" . dirvish))
  :init
  ;; (csetq dired-omit-files
  ;;        (concat dired-omit-files "\\|^\\..*$"))
  (pushnew! savehist-additional-variables 'dirvish--history)
  (dirvish-override-dired-mode)
  :config
  (dirvish-peek-mode 1) ; Preview files in minibuffer

  (csetq dired-dwim-target t
         dired-recursive-copies 'always
         dired-recursive-deletes 'top
         dirvish-preview-dispatchers '(image gif video audio epub pdf archive)
         delete-by-moving-to-trash t)

  (setq dired-mouse-drag-files t) ; added in Emacs 29
  (setq mouse-drag-and-drop-region-cross-program t)

  (csetq dired-listing-switches
         ;; -v - natural sort numbers
         ;; --almost-all - all except . and ..
         (concat "-l -v --almost-all --group-directories-first "
                 "--time-style=long-iso"))

  (csetq dirvish-quick-access-entries
         '(
           ("r" "/"            "Root")
           ("h" "~/"           "Home")
           ("m" "/mnt/"        "Mount")
           ("d" "~/dev/"       "Dev")
           ("s" "~/sync/"      "Sync")
           ("c" "~/.config/"   "Config")
           ("D" "~/Downloads/" "Downloads")
           ("t" "~/.local/share/Trash/files/" "TrashCan")
           ))

  ;; (csetq dired-listing-switches (string-join '("--all"
  ;;                                              "--human-readable"
  ;;                                              "--time-style=long-iso"
  ;;                                              "--group-directories-first"
  ;;                                              "-lv1")
  ;;                                            " "))
  (setq mouse-1-click-follows-link nil)
  (define-key dirvish-mode-map (kbd "<mouse-1>") 'dirvish-subtree-toggle-or-open)
  (define-key dirvish-mode-map (kbd "<mouse-2>") 'dired-mouse-find-file-other-window)
  (define-key dirvish-mode-map (kbd "<mouse-3>") 'dired-mouse-find-file)

  ;; Verbose settings
  (let ((my/file (lambda (path &optional dir)
                   (expand-file-name path (or dir user-emacs-directory))))
        (my/dir (lambda (path &optional dir)
                  (expand-file-name (file-name-as-directory path)
                                    (or dir user-emacs-directory)))))
    (csetq image-dired-thumb-size             150
           image-dired-dir                    (funcall my/dir "dired-img")
           image-dired-db-file                (funcall my/file "dired-db.el")
           image-dired-gallery-dir            (funcall my/dir "gallery")
           image-dired-temp-image-file        (funcall my/file "temp-image" image-dired-dir)
           image-dired-temp-rotate-image-file (funcall my/file "temp-rotate-image" image-dired-dir)))

  (dirvish-define-attribute symlink-arrow
    "Show -> on symlinks but not full target."
    :when (and dired-hide-details-mode
               (default-value 'dired-hide-details-hide-symlink-targets))
    (when (< (+ f-end 4) l-end)
      (let ((ov (make-overlay (+ f-end 4) l-end)))
        (overlay-put ov 'invisible t) ov)))

  (general-pushnew (cons (list "png" "jpg" "jpeg" "webp") (list "mvi" "%f"))
                   dirvish-open-with-programs)

  (csetq dirvish-yank-overwrite-existing-files 'never
         dirvish-attributes '(all-the-icons file-size file-time collapse subtree-state vc-state)
         dirvish-yank-new-name-style 'append-to-filename
         dirvish-yank-new-name-style 'append-to-ext)

  (csetq dirvish-mode-line-position 'global
         dirvish-mode-line-format '(" " file-modes " " file-link-number " " file-user ":" file-group " "
                                    symlink omit vc-info
                                    :right
                                    (sort yank index)))

  (map dirvish-mode-map
       ;; https://github.com/alexluigit/dirvish/issues/186
       "<tab>" #'dirvish-subtree-toggle
       "RET" #'dired-find-file

       "A" #'gnus-dired-attach
       "a" #'dirvish-quick-access
       "b" #'dired-up-directory
       "D" #'dired-do-delete
       "d" #'dirvish-dispatch
       "e" #'dirvish-emerge-menu
       "F" #'dirvish-file-info-menu
       "f" #'dirvish-fd-jump

       ;; "l" #'diredp-find-file-reuse-dir-buffer
       "m" #'dired-mark
       "M" #'dirvish-layout-switch
       "i" #'dired-find-file
       "p" #'dired-previous-line
       "n" #'dired-next-line

       "h" nil
       "hp" #'dirvish-history-go-backward
       "hn" #'dirvish-history-go-forward
       "hj" #'dirvish-history-jump
       "hl" #'dirvish-history-last

       "o" #'dirvish-quicksort
       "O" #'dirvish-chxxx-menu
       "q" #'dirvish-quit
       "s" #'dirvish-quicksort
       "S" #'dirvish-setup-menu
       "t" #'dired-find-file
       "u" #'dired-unmark
       "U" #'dired-unmark-all-marks
       "y" #'dirvish-yank-menu
       "z" #'dirvish-setup-menu

       " " #'dired-next-line

       "C-y" nil
       "C-y p" #'dirvish-yank
       "C-y m" #'dirvish-move
       "C-y l" #'dirvish-symlink
       "C-y L" #'dirvish-relative-symlink
       "C-y h" #'dirvish-hardlink

       "%" nil
       "%u" 'dired-upcase
       "%l" 'dired-downcase
       "%d" 'dired-flag-files-regexp
       "%g" 'dired-mark-files-containing-regexp
       "%m" 'dired-mark-files-regexp
       "%r" 'dired-do-rename-regexp
       "%C" 'dired-do-copy-regexp
       "%H" 'dired-do-hardlink-regexp
       "%R" 'dired-do-rename-regexp
       "%S" 'dired-do-symlink-regexp
       "%&" 'dired-flag-garbage-files

       "*" nil
       "**" 'dired-mark-executables
       "*/" 'dired-mark-directories
       "*@" 'dired-mark-symlinks
       "*%" 'dired-mark-files-regexp
       "*c" 'dired-change-marks
       "*s" 'dired-mark-subdir-files
       "*m" 'dired-mark
       "*u" 'dired-unmark
       "*?" 'dired-unmark-all-files
       "*!" 'dired-unmark-all-marks
       "U" 'dired-unmark-all-marks
       "* <delete>" 'dired-unmark-backward
       "* C-n" 'dired-next-marked-file
       "* C-p" 'dired-prev-marked-file
       )

  ;; truncate long file names instead of wrapping
  (add-hook 'dirvish-find-entry-hook (lambda (&rest _)
                                       (setq-local truncate-lines t)))
  (add-hook 'dirvish-preview-setup-hook (defun dirvish--preview-setup ()
                                          (display-line-numbers-mode -1)
                                          (setq-local mode-line-format nil
                                                      truncate-lines t))))

(provide 'dir)
;;; dir.el ends here.
