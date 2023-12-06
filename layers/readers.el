(require 'core-packages)

(use-package pdf-tools
  :elpaca (:build t)
  :demand t
  :mode (("\\.[pP][dD][fF]\\'" . pdf-view-mode))
  :general
  ('pdf-view-mode-map
           "d" #'pdf-view-previous-page-command
           "h" (lambda () (interactive) (pdf-view-next-line-or-next-page 5))
           "t" (lambda () (interactive) (pdf-view-previous-line-or-previous-page 5))
           "n" #'pdf-view-next-page-command
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
  :config
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
      (pdf-view-last-page)))
  )

;; Biblio package for adding BibTeX records and download publications
(use-package biblio)
(use-package nov)

;; Configure Elfeed
(use-package elfeed
  :config
  (leader :prefix "r"
    "" '(:ignore t :wk "Readers")
    "e" #'elfeed)

  (csetq elfeed-db-directory (concat cache-dir "elfeed/")
         elfeed-show-entry-switch 'display-buffer))

(provide 'readers)
