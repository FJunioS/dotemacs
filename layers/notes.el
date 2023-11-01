;;; notes.el ---  desc  -*- lexical-binding: t; -*-
;;; Commentary:
;; https://www.reddit.com/r/emacs/comments/d7x7x8/finally_fixing_indentation_of_quoted_lists/
;; https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned
;;; Code:
(require 'core-packages)
(require 'core-lib)
(use-package org
  :elpaca '(org :type built-in)
  :general
  ("C-c a" #'org-agenda)
  (leader/agenda
    "l" '(org-agenda-list :wk "week List"))
  ('normal 'org-mode-map
           "C-t" #'+org-toggle-todo-and-fold
           "TAB" #'org-cycle
           "K" #'org-move-subtree-up
           "J" #'org-move-subtree-down
           "L" #'org-demote-subtree
           "H" #'org-promote-subtree
           "SPC '" #'org-edit-src-code)
  ('normal 'org-src-mode-map
           "SPC '" #'org-edit-src-exit
           "SPC k" #'org-edit-src-abort)
  ('org-mode-map
   "C-c C-d" #'+org-toggle-todo-and-fold)
  :config
  (setq org-directory (expand user-notes-dir)
        org-default-notes-file (expand "todo.org" user-notes-dir))

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

  ;; Agenda
  (setq org-agenda-custom-commands
        '(("c" "Calendar" agenda ""
             ((org-agenda-span 7)
              (org-agenda-start-on-weekday 0)
              (org-agenda-time-grid nil)
              (org-agenda-repeating-timestamp-show-all t)
              (org-agenda-entry-types '(:timestamp :sexp))))

          ("d" "Upcoming deadlines" agenda ""
             ((org-agenda-time-grid nil)
              (org-deadline-warning-days 365)
              (org-agenda-entry-types '(:deadline))))

          ("o" "Today" tags-todo "@dev"
             ((org-agenda-overriding-header "Development")
              (org-agenda-skip-function #'my-org-agenda-skip-all-siblings-but-first)))

          ("g" . "GTD contexts")
            ("gd" "Dev" tags-todo "development")
            ("gs" "Computer" tags-todo "studies")
            ("gp" "Projects" tags-todo "projects")))

  (setq org-refile-targets '(("~/notes/todo.org" :maxlevel . 3)
                             ("~/notes/someday.org" :level . 1)
                             ("~/notes/tickler.org" :maxlevel . 2)))

  (setq org-agenda-files `(,(expand "agenda.org" org-directory)
                           "~/notes/todo.org"
                           "~/notes/someday.org"
                           "~/notes/tickler.org"))

  ;; Save Org buffers after refiling!
  ;; Removed: fill `recentf' list
  ;; (advice-add 'org-refile :after 'org-save-all-org-buffers)

  (gsetq org-tag-alist
         '((:startgroup)
           (:endgroup)
           ("@errand" . ?E)
           ("@home" . ?H)
           ("@work" . ?W)
           ("agenda" . ?a)
           ("event" . ?e)
           ("planning" . ?p)
           ("blog" . ?b)
           ("idea" . ?i)))

  (gsetq org-todo-keywords
         '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
           (sequence "EVENT(e)" "|" "TRYST(y)")
           (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))
         org-todo-keyword-faces
         '(("TODO" :foreground "#f9667a" :weight bold)
           ("NEXT" :foreground "#86b2d3" :weight bold)
           ("DONE" :foreground "#4b93ab" :weight bold)
           ("EVENT" :foreground "#bab4fe" :weight bold)
           ("WAITING" :foreground "#f9b37f" :weight bold)
           ("HOLD" :foreground "#bab4fe" :weight bold)
           ("CANCELLED" :foreground "#a3bbae" :weight bold)
           ("MEETING" :foreground "#a3bbae" :weight bold)
           ("PHONE" :foreground "#a3bbae" :weight bold)))

  (add-hook 'kill-emacs-hook #'ju/org--clock-out)
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

  (defun ju/org--clock-out()
    (org-clock-out nil t))

  (defun my-org-agenda-skip-all-siblings-but-first ()
    "Skip all but the first non-done entry."
    (let (should-skip-entry)
      (unless (org-current-is-todo)
        (setq should-skip-entry t))
      (save-excursion
        (while (and (not should-skip-entry) (org-goto-sibling t))
          (when (org-current-is-todo)
            (setq should-skip-entry t))))
      (when should-skip-entry
        (or (outline-next-heading)
            (goto-char (point-max))))))

  (defun org-current-is-todo ()
    (string= "TODO" (org-get-todo-state))))

;;

(use-package org-noter)
(use-package org-drill)
(use-package org-appear
  :hook (org-mode-hook . org-appear-mode))
(defvar emacs-assets-dir (expand "assets/" emacs-dir))

(use-package org-pomodoro
  :commands (org-pomodoro-start org-pomodoro)
  :config
  ;; Run timer again after finishing
  (add-hook 'org-pomodoro-break-finished-hook #'(lambda () (run-with-timer 5 nil #'org-pomodoro-start)))
  ;; Send visual notification when a timer ends
  (setq
   alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil))))
  (gsetq org-pomodoro-format "%s"
         org-pomodoro-length 0.1
         org-pomodoro-start-sound (expand "bells.wav" emacs-assets-dir)
         org-pomodoro-finished-sound (expand "bells.wav" emacs-assets-dir)
         org-pomodoro-overtime-sound (expand "bells.wav" emacs-assets-dir)
         org-pomodoro-long-break-sound (expand "singing-bowl.wav" emacs-assets-dir)
         org-pomodoro-short-break-sound (expand "singing-bowl.wav" emacs-assets-dir))
  org-pomodoro-clock-break t)

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
  (org-superstar-item-bullet-alist '((?* . ?•) (?+ . ?▹) (?- . ?◦)))) ; changes +/- symbols in item lists

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

(general-def 'org-read-date-minibuffer-local-map
  "J" (general-simulate-key "S-<down>")
  "K" (general-simulate-key "S-<up>")
  "L" (general-simulate-key "S-<right>")
  "H" (general-simulate-key "S-<left>"))

(general-def 'normal 'calendar-mode-map
  "b" 'calendar-backward-day
  "h" 'calendar-backward-day
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
  "RET" 'org-calendar-select
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
  "q" 'calendar-exit)

(provide 'notes)
;;; notes.el ends here
