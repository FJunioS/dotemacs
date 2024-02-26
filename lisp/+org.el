;; -*- lexical-binding: t; -*-

(require 'org)

(defun +org-toggle-todo-and-fold ()
  (interactive)
  (save-excursion
    (org-back-to-heading t) ;; Make sure command works even if point is
    ;; below target heading
    (cond ((looking-at "\*+ TODO")
           (org-todo "DONE")
           (hide-subtree))
          ((looking-at "\*+ DONE")
           (org-todo "TODO")
           (hide-subtree))
          (t (message "Can only toggle between TODO and DONE.")))))

(defun +org-cycle-only-current-subtree-h (&optional arg)
  "Toggle the local fold at the point, and no deeper.
                  `org-cycle's standard behavior is to cycle between three levels: collapsed,
                  subtree and whole document. This is slow, especially in larger org buffer. Most
                  of the time I just want to peek into the current subtree -- at most, expand
                  *only* the current subtree.

                  All my (performant) foldings needs are met between this and `org-show-subtree'
                  (on zO for evil users), and `org-cycle' on shift-TAB if I need it."
  (interactive "P")
  (unless (or (eq this-command 'org-shifttab)
              (and (bound-and-true-p org-cdlatex-mode)
                   (or (org-inside-LaTeX-fragment-p)
                       (org-inside-latex-macro-p))))
    (save-excursion
      (org-beginning-of-line)
      (let (invisible-p)
        (when (and (org-at-heading-p)
                   (or org-cycle-open-archived-trees
                       (not (member org-archive-tag (org-get-tags))))
                   (or (not arg)
                       (setq invisible-p (outline-invisible-p (line-end-position)))))
          (unless invisible-p
            (setq org-cycle-subtree-status 'subtree))
          (org-cycle-internal-local)
          t)))))

(provide '+org)
;;; +org.el ends here.
