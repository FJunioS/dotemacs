(require 'core-packages)

(use-package dired
  :elpaca nil
  :general (leader "-" #'dired-jump)
  :config
  (defun dired--recentf-fix (orig-fun &rest args)
    (let ((recentf-exclude '(".*")))
      (apply orig-fun args)))
  (advice-add 'dired-up-directory :around
              #'dired--recentf-fix))

(use-package dired-rsync
  :if (executable-find "rsync")
  :defer t
  :general
  (:keymaps 'dired-mode-map
            "C-r" #'dired-rsync))

(use-package dirvish
  :init
  ;; (gsetq dired-omit-files
  ;;        (concat dired-omit-files "\\|^\\..*$"))
  (dirvish-override-dired-mode)

  :config
  (gsetq dired-kill-when-opening-new-dired-buffer nil
         dired-clean-confirm-killing-deleted-buffers nil
         dired-dwim-target t
         dired-recursive-copies 'always
         dired-recursive-deletes 'top
         delete-by-moving-to-trash t)

  (setq dired-mouse-drag-files t) ; added in Emacs 29
  (setq mouse-drag-and-drop-region-cross-program t)
  (gsetq dired-listing-switches
         ;; -v - natural sort numbers
         ;; --almost-all - all except . and ..
         (concat "-l -v --almost-all --group-directories-first "
                 "--time-style=long-iso"))

  (gsetq dirvish-quick-access-entries
         '(("h" "~/"                          "Home")
           ("d" "~/Downloads/"                "Downloads")
           ("m" "/mnt/"                       "Drives")
           ("t" "~/.local/share/Trash/files/" "TrashCan")))

  ;; (gsetq dired-listing-switches (string-join '("--all"
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
    (gsetq image-dired-thumb-size             150
           image-dired-dir                    (funcall my/dir "dired-img")
           image-dired-db-file                (funcall my/file "dired-db.el")
           image-dired-gallery-dir            (funcall my/dir "gallery")
           image-dired-temp-image-file        (funcall my/file "temp-image" image-dired-dir)
           image-dired-temp-rotate-image-file (funcall my/file "temp-rotate-image" image-dired-dir)))


  ;; (dirvish-define-attribute symlink-arrow
  ;;   "Show -> on symlinks but not full target."
  ;;   :when (and dired-hide-details-mode
  ;;              (default-value 'dired-hide-details-hide-symlink-targets))
  ;;   (when (< (+ f-end 4) l-end)
  ;;     (let ((ov (make-overlay (+ f-end 4) l-end)))
  ;;       (overlay-put ov 'invisible t) ov)))

  (general-pushnew (cons (list "png" "jpg" "jpeg" "webp") (list "mvi" "%f"))
                   dirvish-open-with-programs)

  (gsetq dirvish-yank-overwrite-existing-files 'never
         dirvish-attributes '(all-the-icons file-size collapse vc-state)
         dirvish-yank-new-name-style 'append-to-filename
         dirvish-yank-new-name-style 'append-to-ext)

  (gsetq dirvish-mode-line-position 'global
         dirvish-mode-line-format '(" " file-modes " " file-link-number " " file-user ":" file-group " "
                                    symlink omit vc-info
                                    :right
                                    (sort yank index)))

  ;; (gsetq dirvish-header-line-format
  ;;        '(:left (path symlink)
  ;;                :right (free-space))
  ;;        dirvish-layout-recipes
  ;;        (list '(0 0 0.8)
  ;;              '(0 0 0.4)
  ;;              dirvish-default-layout))
  :general
  (def 'dirvish-mode-map
       ;; https://github.com/alexluigit/dirvish/issues/186
       "<tab>" #'dirvish-subtree-toggle

       "A" #'gnus-dired-attach
       "a" #'dirvish-quick-access
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
       "n" (lambda () (interactive)
             (progn (dired-previous-line 1)
                    (dired-next-line 1)))
       "h" '(:ignore t :which-key "history")
       "hp" #'dirvish-history-go-backward
       "hn" #'dirvish-history-go-forward
       "hj" #'dirvish-history-jump
       "hl" #'dirvish-history-last

       "o" #'dirvish-quicksort
       " " #'dired-next-line

       "C-y" '(:ignore t :which-key "paste")
       "C-y p" #'dirvish-yank
       "C-y m" #'dirvish-move
       "C-y l" #'dirvish-symlink
       "C-y L" #'dirvish-relative-symlink
       "C-y h" #'dirvish-hardlink

       "q" #'dirvish-quit

       "s" #'dirvish-quicksort
       "S" #'dirvish-setup-menu

       "u" #'dired-unmark
       "U" #'dired-unmark-all-marks

       "%" '(:ignore t :wk "Regexp")
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

       "*" '(:ignore t :wk "Mark files")
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

       "y"   #'dirvish-yank-menu
       "y"  '(:ignore t :which-key "yank")
       "yp" #'dirvish-copy-file-path
       "yn" #'dirvish-copy-file-name
       "yd" #'dirvish-copy-file-directory

       "z"  #'dirvish-setup-menu)

  :gfhook
  ;; truncate long file names instead of wrapping
  ('dirvish-find-entry-hook
   (lambda (&rest _) (setq-local truncate-lines t)))
  ('dirvish-preview-setup-hook (defun dirvish--preview-setup ()
                                 (display-line-numbers-mode -1)
                                 (setq-local mode-line-format nil
                                             truncate-lines t))))

(provide 'dir)
;; dir.el ends here
