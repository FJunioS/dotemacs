;;; notes.el ---  desc  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'core-packages)
(require 'core-lib)

(use-package org
  :straight '(org :type built-in)
  :general
  ('normal 'org-mode-map
    "tt" #'+org-toggle-todo-and-fold
    "SPC '" #'org-edit-src-code)
  ('normal 'org-src-mode-map
    "SPC '" #'org-edit-src-exit
    "SPC k" #'org-edit-src-abort)
  ('org-mode-map
    "C-c C-d" #'+org-toggle-todo-and-fold)
  :config
    (defvar org-directory (expand user-notes-dir))
    (defvar org-notes (expand user-notes-dir))

    (require '+org)

    (setq org-tab-first-hook #'+org-cycle-only-current-subtree-h)

    ;; display images
    (setq org-display-remote-inline-images t
          org-startup-with-inline-images t
          org-cycle-inline-images-display t)

    ;; better default
    (setq org-catch-invisible-edits nil
          org-hide-emphasis-markers t
          org-return-follows-link t
          org-enforce-todo-dependencies t)

    ;; indent
    (setq org-startup-folded t
          org-startup-indented t
          org-list-indent-offset 2
          org-pretty-entities t
          org-return-follows-link t
          org-cycle-separator-lines 2)

    (add-hook 'kill-emacs-hook #'ju/org--clock-out)

    ;; Save Org buffers after refiling!
    ;; Removed: fill `recentf' list
    ;; (advice-add 'org-refile :after 'org-save-all-org-buffers)

    (setq org-tag-alist
          '((:startgroup)
            (:endgroup)
            ("@errand" . ?E)
            ("@home" . ?H)
            ("@work" . ?W)
            ("agenda" . ?a)
            ("planning" . ?p)
            ("blog" . ?b)
            ("emacs" . ?e)
            ("note" . ?n)
            ("idea" . ?i)))

    (setq org-todo-keywords
          (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                  (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
          org-todo-keyword-faces
          '(("TODO" :foreground "red" :weight bold)
            ("NEXT" :foreground "blue" :weight bold)
            ("DONE" :foreground "forest green" :weight bold)
            ("WAITING" :foreground "orange" :weight bold)
            ("HOLD" :foreground "magenta" :weight bold)
            ("CANCELLED" :foreground "forest green" :weight bold)
            ("MEETING" :foreground "forest green" :weight bold)
            ("PHONE" :foreground "forest green" :weight bold)))

    :preface
    (defun ju.org-toggle-todo-and-fold ()
      (interactive)
      (save-excursion
        (org-back-to-heading t) ;; Make sure command works even if point is
        ;; below target heading
        (cond ((looking-at "\*+ TODO")
               (org-todo "DONE")
               (outline-hide-subtree))
              ((looking-at "\*+ DONE")
               (org-todo "TODO")
               (outline-hide-subtree))
              (t (message "Can only toggle between TODO and DONE.")))))
    (defun ju/org--clock-out() (org-clock-out nil t)))

;;

(use-package org-appear
  :hook (org-mode . org-appear-mode))

(use-package org-journal
  :general
  (leader/notes
    "t" '(org-journal-open-current-journal-file :wk "Today's journal")
    "o" '(org-journal-new-entry :wk "Open journal"))
  :config
  (setq
   org-journal-date-prefix "#+title: "
   org-journal-file-format "%Y-%m-%d.org"
   org-journal-dir (expand "private/journal" user-notes-dir)
   org-journal-date-format "%A, %d %B %Y"))

(use-package org-superstar
  :ghook 'org-mode-hook
  :custom
  (set-face-attribute 'org-superstar-item nil :height 0.8)
  (set-face-attribute 'org-superstar-header-bullet nil :height 0.8)
  (set-face-attribute 'org-superstar-leading nil :height 0.8)
  (org-ellipsis " ⋯")
  (org-superstar-special-todo-items t)
  (org-superstar-headline-bullets-list '("" "•" "•" "•" "•" "•")) ;;  use char `⁖' if first symbol don't work
  (org-superstar-item-bullet-alist '((?* . ?•) (?+ . ?▹) (?- . ?◦))) ; changes +/- symbols in item lists

  (org-superstar-todo-bullet-alist
   ;; Enable custom bullets for TODO items
   '(("TODO:" . ?☐)
     ("NEXT:" . ?✒)
     ("HOLD:" . ?✰)
     ("WAITING:" . ?☕)
     ("CANCELLED:" . ?✘)
     ("DONE:" . ?✔))))

(use-package toc-org
  :ghook 'org-mode-hook 'markdown-mode-hook
  :config
  ;; enable in markdown, too
  (add-hook 'markdown-mode-hook 'toc-org-mode))

(use-package org-roam
  :general
  (leader/notes
    "n" '(org-roam-capture :wk "Capture")
    "f" '(org-roam-node-find :wk "Find node"))
  (leader/mode 'org-mode-map
	  "a" #'org-roam-alias-add
	  "r" #'org-roam-ref-add)
  :config
  (setq org-roam-db-gc-threshold most-positive-fixnum)
  (setq org-roam-directory (expand user-notes-dir))
  (setq org-id-extra-files (org-roam--list-files org-roam-directory))


  (setq org-roam-capture-templates
        '(("d" "default" plain "%?"
           :immediate-finish t
           :if-new (file+head "${slug}.org"
                              "#+TITLE: ${title}\n#+lastmod: Time-stamp: <>\n\n")
           :unnarrowed t)
          ("t" "temp" plain "%?"
           :if-new(file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                             "#+TITLE: ${title}\n#+lastmod: Time-stamp: <>\n\n")
           :immediate-finish t
           :unnarrowed t)
          ("p" "private" plain "%?"
           :if-new (file+head "${slug}-private.org"
                              "#+TITLE: ${title}\n")
           :immediate-finish t
           :unnarrowed t)))

  (setq org-roam-node-display-template
        (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))

  (setq org-roam-completion-system 'vertico
        org-roam-completion-everywhere t
        org-roam-mode-sections
        '(#'org-roam-backlinks-section
          #'org-roam-reflinks-section
          #'org-roam-unlinked-references-section))

  (org-roam-db-autosync-mode))

(use-package org-download
  :config
  (setq org-download-display-inline-images t
        org-download-image-dir (expand "_img" user-notes-dir))
  :gfhook
  ('dired-mode-hook 'org-download-enable))

;; Add timestamp on capture notes
(defun my-org-roam-update-timestamp ()
  "Update timestamp in Org Roam buffer."
  (interactive)
  (when (eq major-mode 'org-mode)
    (time-stamp)
    (save-buffer t)))

(add-hook 'kill-buffer-hook 'my-org-roam-update-timestamp)

(setq org-capture-templates
      '((?n "Notes" entry
            (file "~/sync/notes/inbox.org") "* %^{Description} %^g\n Added: %U\n%?")
        (?m "Meeting notes" entry
            (file "~/sync/notes/meetings.org") "* TODO %^{Title} %t\n- %?")
        (?t "TODO" entry
            (file "~/sync/notes/inbox.org") "* TODO %^{Title}")
        (?e "Event" entry
            (file "~/sync/notes/calendar.org") "* %^{Is it a todo?||TODO |NEXT }%^{Title}\n%^t\n%?")
        (?w "Work TODO" entry
            (file "~/sync/notes/work.org") "* TODO %^{Title}")))
;;

;; Calendar
(with-eval-after-load 'evil-mode
  (evil-set-initial-state 'calendar-mode 'normal))

(general-def 'normal 'calendar-mode-map
  "h" 'calendar-backward-day
  "b" 'calendar-backward-day
  "j" 'calendar-forward-week
  "k" 'calendar-backward-week
  "l" 'calendar-forward-day
  "w" 'calendar-forward-day
  "0" 'calendar-beginning-of-week
  "^" 'calendar-beginning-of-week
  "$" 'calendar-end-of-week
  "-" 'calendar-end-of-week
  "[[" 'calendar-backward-year
  "]]" 'calendar-forward-year
  "M-<" 'calendar-beginning-of-year
  "M->" 'calendar-end-of-year
  "(" 'calendar-beginning-of-month
  ")" 'calendar-end-of-month
  "SPC" 'scroll-other-window
  "S-SPC" 'scroll-other-window-down
  "<delete>" 'scroll-other-window-down
  "<" 'calendar-scroll-right
  ">" 'calendar-scroll-left
  "C-b" 'calendar-scroll-right-three-months
  "C-f" 'calendar-scroll-left-three-months
  "{" 'calendar-backward-month
  "}" 'calendar-forward-month
  "C-k" 'calendar-backward-month
  "C-h" 'calendar-backward-month
  "C-j" 'calendar-forward-month
  "C-l" 'calendar-forward-month
  "gk" 'calendar-backward-month
  "gj" 'calendar-forward-month

  ;; visual
  "v" 'calendar-set-mark

  ;; goto
  "." 'calendar-goto-today
  "o" 'calendar-other-month
  "gd" 'calendar-goto-date ; "gd" in evil-org-agenda, "gd" in Emacs.
  "gD" 'calendar-other-month

  ;; diary
  "D" 'diary-view-other-diary-entries
  "d" 'diary-view-entries
  "m" 'diary-mark-entries
  "s" 'diary-show-all-entries

  ;; appointment
  "Aa" 'appt-add
  "Ad" 'appt-delete

  "u" 'calendar-unmark
  "x" 'calendar-mark-holidays

  ;; show
  "gm" 'calendar-lunar-phases ; "gm" in evil-org-agenda. TODO: Shadows calendar-mayan.
  "gs" 'calendar-sunrise-sunset ; "gs" in evil-org-agenda
  "gh" 'calendar-list-holidays ; "gh" in evil-org-agenda. TODO: Shadows calendar-hebrew.
  "gc" 'org-calendar-goto-agenda ; "gc" in evil-org-agenda. TODO: Shadows calendar-iso.
  "a" 'calendar-list-holidays
  "r" 'calendar-cursor-holidays

  ;; refresh
  "gr" 'calendar-redraw

  "g?" 'calendar-goto-info-node
  "?" 'calendar-goto-info-node ; Search is not very useful.
  "M-=" 'calendar-count-days-region

  ;; quit
  "q" 'calendar-exit
  "ZQ" 'evil-quit
  "ZZ" 'calendar-exit)

(provide 'notes)
;;; notes.el ends here
