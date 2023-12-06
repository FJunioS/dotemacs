;;; notes.el ---  desc  -*- lexical-binding: t; -*-
;;; Commentary:
;; https://www.reddit.com/r/emacs/comments/d7x7x8/finally_fixing_indentation_of_quoted_lists/
;; https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned
;;; Code:
(require 'core-packages)
(require 'core-lib)

;; Denote extensions
(use-package consult-notes
  :ensure t
  :after denote
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :init
  (when (require 'consult-notes nil t)
    (csetq consult-notes-file-dir-sources (list `("All" 'a "~/notes")))
    (consult-notes-denote-mode))
  (global-key
   "C-c nf" #'consult-notes
   "C-c ns" #'consult-notes-search-in-all-notes))

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
  :ensure t
  :init
  (require 'denote-org-dblock)
  (denote-rename-buffer-mode t)
  :hook
  (dired-mode . denote-dired-mode)
  :custom-face
  (denote-faces-link ((t (:slant italic))))
  :custom
  (denote-known-keywords '("rust"
                           "emacs"
                           "computer-science"
                           "philosophy"
                           "politics"
                           "economics"))
  :config
  (csetq denote-directory user-notes-dir)
  (map leader-map
       "n n" #'denote-create-note
       "n j" #'denote-journal-extras-new-or-existing-entry
       "n d" #'denote-date
       "n i" #'denote-link-or-create
       "n l" #'denote-find-link
       "n b" #'denote-find-backlink
       "n D" #'denote-org-dblock-insert-links
       "n r" #'denote-rename-file-using-front-matter
       "n R" #'denote-rename-file
       "n k" #'denote-keywords-add
       "n K" #'denote-keywords-remove))

(use-package org
  :elpaca '(org :type built-in)
  :general
  ("C-c a" #'org-agenda)
    (general-def 'normal 'org-mode-map
    "C-t" #'+org-toggle-todo-and-fold
    "TAB" #'org-cycle
    "K" #'org-move-subtree-up
    "J" #'org-move-subtree-down
    "L" #'org-demote-subtree
    "H" #'org-promote-subtree
    "SPC '" #'org-edit-src-code)
  (general-def 'normal 'org-src-mode-map
    "SPC '" #'org-edit-src-exit
    "SPC k" #'org-edit-src-abort)
  (general-def 'org-mode-map
    "C-," #'execute-extended-command
    "C-c C-d" #'+org-toggle-todo-and-fold)
  :config
  (csetq org-capture-bookmark nil)
  (setq require-final-newline t)
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
  (csetq org-tag-alist
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

  (csetq org-todo-keywords
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
  :ensure t
  :after org
  :commands (org-pomodoro-start org-pomodoro)
  :bind (("<f12>" . work-pomodoro))
  :hook ((org-pomodoro-started . ju/load-window-close-agenda)
         (org-pomodoro-finished . ju/save-window-show-agenda))
  :config
  ;; Send visual notification when a timer ends
  (csetq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil))))
  (csetq org-pomodoro-format "%s"
         org-pomodoro-start-sound (expand "bells.wav" emacs-assets-dir)
         org-pomodoro-finished-sound (expand "bells.wav" emacs-assets-dir)
         org-pomodoro-overtime-sound (expand "bells.wav" emacs-assets-dir)
         org-pomodoro-long-break-sound (expand "singing-bowl.wav" emacs-assets-dir)
         org-pomodoro-short-break-sound (expand "singing-bowl.wav" emacs-assets-dir)
         org-pomodoro-clock-break t
         org-pomodoro-manual-break t)

  (defun work-pomodoro ()
    (interactive)
    (csetq org-pomodoro-length 50
           org-pomodoro-short-break-length 20)
    (if (not (org-pomodoro-active-p))
        (progn
          (org-pomodoro-start)
          (message "Pomodoro started/unpaused"))
      (progn
        (org-pomodoro t)
        (message "Pomodoro Paused"))))

  (defun ju/save-window-show-agenda ()
    (interactive)
    (window-configuration-to-register "`")
    (delete-other-windows)
    (org-save-all-org-buffers)
    (org-agenda nil "w"))

  (defun ju/load-window-close-agenda ()
    (interactive)
    (org-save-all-org-buffers)
    (jump-to-register "`")))

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
  :hook (org-mode .  org-superstar-mode)
  :disabled t
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
  :disabled t
  :general
  (leader/notes
    "n" '(org-roam-capture :wk "Capture")
    "f" '(org-roam-node-find :wk "Find node"))
  ( 'org-mode-map
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

;; Calendar
(with-eval-after-load 'evil-mode
  (evil-set-initial-state 'calendar-mode 'normal))

(provide 'notes)
;;; notes.el ends here
