(require 'core-packages)


(use-package pdf-tools
  :elpaca (:build t)
  :demand t
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
  :gfhook #'pdf-view-midnight-minor-mode
  :general
  ('normal 'pdf-view-mode-map
           "h" #'pdf-view-previous-page-command
           "j" (lambda () (interactive) (pdf-view-next-line-or-next-page 5))
           "k" (lambda () (interactive) (pdf-view-previous-line-or-previous-page 5))
           "l" #'pdf-view-next-page-command
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
  (add-hook 'pdf-tools-enabled-hook (lambda () (midnight-mode 1))))

(provide 'readers)
