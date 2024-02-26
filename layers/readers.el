;; -*- lexical-binding: t; -*-

(require 'core-packages)

(use-package pdf-tools
  :ensure t
  ;; I use nix bin instead of compiling
  :init
  (csetq-default doc-view-resolution nil)
  (pdf-loader-install t)
  :mode (("\\.[pP][dD][fF]\\'" . pdf-view-mode))
  :config
  (map pdf-view-mode-map
       "d" #'pdf-view-previous-page-command
       "h" (lambda () (interactive) (pdf-view-next-line-or-next-page 5))
       "t" (lambda () (interactive) (pdf-view-previous-line-or-previous-page 5))
       "n" #'pdf-view-next-page-command
       "C-#" #'pdf-view-enlarge
       "C-!" #'pdf-view-shrink
       ;; alternatively
       "g" #'image-bob
       "G" #'noct:pdf-view-goto-page
       "m" #'pdf-view-position-to-register
       "'" #'pdf-view-jump-to-register
       "/" #'pdf-occur
       "o" #'pdf-outline
       "f" #'pdf-links-action-perform
       "b" #'pdf-view-midnight-minor-mode
       "y" 'core:pdf-view-page-as-text
       "C-o" #'pdf-history-backward
       "C-i" #'pdf-history-forward)

  ;; Ediff
  (require 'ediff)
  (csetq ediff-split-window-function #'split-window-vertically
         ediff-window-setup-function #'ediff-setup-windows-plain)

  (add-hook 'pdf-tools-enabled-hook #'pdf-view-midnight-minor-mode)

  :preface
  (defun core:pdf-view-page-as-text ()
    "Inserts current pdf page into a buffer for keyboard selection."
    (interactive)
    (pdf-view-mark-whole-page)
    (pdf-view-kill-ring-save)
    (switch-to-buffer (make-temp-name "pdf-page"))
    (save-excursion
      (yank)))
  (defun noct:pdf-view-goto-page (count)
    "Goto page COUNT.
  If COUNT is not supplied, go to the last page."
    (interactive "P")
    (if count
        (pdf-view-goto-page count)
      (pdf-view-last-page))))

;; Biblio package for adding BibTeX records and download publications
(use-package biblio)
(use-package nov)

;; Configure Elfeed
(use-package elfeed
  :ensure t
  :init
  (map leader-map
       "re" #'elfeed)

  (csetq elfeed-db-directory (concat cache-dir "elfeed/")
         elfeed-show-entry-switch 'display-buffer)
  (add-hook 'elfeed-new-entry-hook #'elfeed-declickbait-entry)

  (defun elfeed-declickbait-entry (entry)
    (let ((title (elfeed-entry-title entry)))
      (setf (elfeed-meta entry :title)
            (elfeed-title-transform title))))

  (defun elfeed-title-transform (title)
    "Declickbait string TITLE."
    (let* ((trim "\\(?:\\(?:\\.\\.\\.\\|[!?]\\)+\\)")
           (arr (split-string title nil t trim))
           (s-table (copy-syntax-table)))
      (modify-syntax-entry ?\' "w" s-table)
      (with-syntax-table s-table
        (mapconcat (lambda (word)
                     (cond
                      ((member word '("AND" "OR" "IF" "ON" "IT" "TO"
                                      "A" "OF" "VS" "IN" "FOR" "WAS"
                                      "IS" "BE"))
                       (downcase word))
                      ((member word '("WE" "DAY" "HOW" "WHY" "NOW" "OLD"
                                      "NEW" "MY" "TOO" "GOT" "GET" "THE"
                                      "ONE" "DO" "YOU"))
                       (capitalize word))
                      ((> (length word) 3) (capitalize word))
                      (t word)))
                   arr " "))))
  )

(provide 'readers)

(provide 'readers)
;;; readers.el ends here.
