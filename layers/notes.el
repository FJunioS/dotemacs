;;; notes.el ---  desc  -*- lexical-binding: t; -*-
;;; Commentary:
;; https://www.reddit.com/r/emacs/comments/d7x7x8/finally_fixing_indentation_of_quoted_lists/
;; https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned
;;; Code:
(require 'core-packages)
(require 'core-lib)

;; Configure Elfeed
(use-package elfeed
  :config
  (leader :prefix "r"
    "" '(:ignore t :wk "Readers")
    "e" #'elfeed)

  (gsetq elfeed-db-directory (concat cache-dir "elfeed/")
         elfeed-show-entry-switch 'display-buffer))

;; Denote extensions
(use-package consult-notes
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :bind (("C-c nf" . consult-notes)
         ("C-c ns" . consult-notes-search-in-all-notes)))

;; Easy insertion of weblinks
(use-package org-web-tools)

(use-package persistent-scratch
  :hook
  (after-init . persistent-scratch-setup-default)
  :init
  (persistent-scratch-setup-default)
  (persistent-scratch-autosave-mode)
  :general
  ("C-c x r" #'scratch-buffer))

;; Modernise Org mode interface
(use-package olivetti
  :demand t
  :ghook '(org-mode-hook text-mode-hook))

(use-package denote
  :init
  (require 'denote-org-dblock)
  (denote-rename-buffer-mode t)
  :hook
  (dired-mode . denote-dired-mode)
  :custom-face
  (denote-faces-link ((t (:slant italic))))
  :custom
  (denote-directory user-notes-dir)
  (denote-known-keywords '("rust"
                           "emacs"
                           "computer-science"
                           "philosophy"
                           "politics"
                           "economics"))
  :bind
  (("C-c w n" . denote-create-note)
   ("C-c w j" . denote-date)
   ("C-c w i" . denote-link-or-create)
   ("C-c w l" . denote-find-link)
   ("C-c w b" . denote-find-backlink)
   ("C-c w D" . denote-org-dblock-insert-links)
   ("C-c w r" . denote-rename-file-using-front-matter)
   ("C-c w R" . denote-rename-file)
   ("C-c w k" . denote-keywords-add)
   ("C-c w K" . denote-keywords-remove)))


(use-package org
  :elpaca '(org :type built-in)
  :general
  ("C-c aa" #'org-agenda)
  ("C-c al" '(org-agenda-list :wk "week List"))
  (general-def 'org-mode-map
           "C-c C-d" #'+org-toggle-todo-and-fold
           "TAB" #'org-cycle)
  :config
  (setq org-capture-bookmark nil)
  (setq require-final-newline t)
  (setq org-directory (expand user-notes-dir)
        consult-notes-file-dir-sources `(("Notes" "n" ,user-notes-dir))
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

  (setq org-todo-keywords
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
  :hook (org-mode . org-appear-mode))

(defvar emacs-assets-dir (expand "assets/" emacs-dir))

(use-package org-pomodoro
  :commands (org-pomodoro-start org-pomodoro)
  :config
  (setq
   alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil))))
  (setq org-pomodoro-format "%s"
         org-pomodoro-length 0.1
         Org-pomodoro-start-sound (expand "bells.wav" emacs-assets-dir)
         org-pomodoro-finished-sound (expand "bells.wav" emacs-assets-dir)
         org-pomodoro-overtime-sound (expand "bells.wav" emacs-assets-dir)
         org-pomodoro-long-break-sound (expand "singing-bowl.wav" emacs-assets-dir)
         org-pomodoro-short-break-sound (expand "singing-bowl.wav" emacs-assets-dir)
         org-pomodoro-clock-break t))

(use-package org-journal
  :general
  ("C-c nt" '(org-journal-open-current-journal-file :wk "Today's journal")
   "C-c no" '(org-journal-new-entry :wk "Open journal"))
  :config
  (setq
   org-journal-date-prefix "#+title: "
   org-journal-file-format "%Y-%m-%d.org"
   org-journal-dir (expand "private/journal" user-notes-dir)
   org-journal-date-format "%A, %d %B %Y"))

(use-package org-superstar
  :hook (org-mode .  org-superstar-mode)
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

(use-package org-download
  :config
  (setq org-download-display-inline-images t
        org-download-image-dir (expand "_img" user-notes-dir))
  :gfhook
  ('dired-mode-hook 'org-download-enable))

(setq org-capture-templates
      '((?n "Notes" entry
            (file "inbox.org") "* %^{Description} %^g\n Added: %U\n%?")
        (?m "Meeting notes" entry
            (file "meetings.org") "* TODO %^{Title} %t\n- %?")
        (?t "TODO" entry
            (file "inbox.org") "* TODO %^{Title}")
        (?e "Event" entry
            (file "calendar.org") "* %^{Is it a todo?||TODO |NEXT }%^{Title}\n%^t\n%?")
        (?w "Work TODO" entry
            (file "work.org") "* TODO %^{Title}")))

(general-def 'org-read-date-minibuffer-local-map
  "J" (general-simulate-key "S-<down>")
  "K" (general-simulate-key "S-<up>")
  "L" (general-simulate-key "S-<right>")
  "H" (general-simulate-key "S-<left>"))

(provide 'notes)
;;; notes.el ends here
