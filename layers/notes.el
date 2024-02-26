;; -*- lexical-binding: t; -*-
(require '+org)

(use-package org
  :ensure
  :config
  (map ju-notes-map
       "c" #'org-capture)
  (map org-mode-map
       "C-t" 'ju-menu-map
       "C-," #'execute-extended-command
       "C-c d" #'org-todo
       "C-c h" #'consult-org-heading
       "C-c TAB" #'mode-line-other-buffer
       "C-c C-t" #'+org-toggle-todo-and-fold
       "C-c C-d" #'org-babel-demarcate-block
       "TAB" #'org-cycle
       "<up>" #'org-move-subtree-up
       "<down>" #'org-move-subtree-down
       "<left>" #'org-demote-subtree
       "<right>" #'org-promote-subtree)

  ;; Org Src editing goes on the side (better to edit)
  (noct-handle-popup (rx "*Org Src " (+ any) "*" eol) nil right)

  (csetq org-capture-bookmark nil)
  (setq require-final-newline t)
  (setq org-directory user-notes-dir
        org-default-notes-file (expand "00000000T000011--notes.org" user-notes-dir))

  ;; run after a resettable delay of 0.3 seconds.
  ;;  (debounce! 'org-agenda-do-context-action 0.3)
  (setq org-tab-first-hook #'+org-cycle-only-current-subtree-h)

  (setq org-use-speed-commands t)

  ;; Allow org snippets with `<`
  (add-to-list 'org-modules 'org-tempo)
  (setq org-structure-template-alist
        '(("c" . "comment")
          ("e" . "src elisp")
          ("E" . "example")
          ("h" . "export html")
          ("l" . "export latex")
          ("q" . "quote")
          ("s" . "src")
          ("r" . "src rust")
          ("x" . "export")))

  ;; display images
  (setq org-display-remote-inline-images t
        org-startup-with-inline-images t
        org-cycle-inline-images-display t)

  ;; better default
  (setq org-catch-invisible-edits nil
        org-hide-emphasis-markers t
        org-enforce-todo-dependencies t)

  ;; indent
  (setq org-startup-folded t
        org-startup-indented t
        org-list-indent-offset 2
        org-pretty-entities t
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

  (setq org-agenda-files (list (concat org-directory "00000000T000001--agenda.org")))

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

  ;; Babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((C . t)
     (sql . t)
     (python . t)
     (emacs-lisp . t)))
  (setq-default org-confirm-babel-evaluate nil
                 org-src-fontify-natively t
                 org-src-tab-acts-natively t)

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
(size-indication-mode)

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
  (string= "TODO" (org-get-todo-state)))
) ;; End of `use-package org`
;; support to append notes on PDF, EPub and others
(use-package org-noter
  :ensure t
  :custom
  ;; Save the last location you visited and pick it up when you start another session
  (org-noter-auto-save-last-location t))

(use-package org-special-block-extras
  :hook (org-mode . org-special-block-extras-mode)
  :config
  (org-defblock noweb-template
                (:tangle "no" :backend emacs-lisp :noweb yes :noweb-ref nil)
                "Generate a template for config"))

(use-package org-modern
  :ensure t
  :hook (org-mode . global-org-modern-mode)
  :config
  (set-face-attribute 'org-modern-symbol nil :family fixed-pitch-font))

(use-package org-pdftools
  :after org-noter)

;; A kind of spaced repetition tool
(use-package org-drill)

;; Show marks on cursor
(use-package org-appear
  :ensure t
  :init
  (add-hook 'org-mode-hook 'org-appear-mode))

(defvar emacs-assets-dir (expand "assets/" emacs-dir))

(use-package org-pomodoro
  :ensure t
  :after org
  :commands (org-pomodoro-start org-pomodoro)
  :bind (("<f12>" . work-pomodoro))
  ;; :hook ((org-pomodoro-started . ju/load-window-close-agenda)
  ;;        (org-pomodoro-finished . ju/save-window-show-agenda))
  :config
  ;; Send visual notification when a timer ends
  (csetq alert-user-configuration '(((((:category . "org-pomodoro")) libnotify nil))))
  (csetq org-pomodoro-format "%s"
         org-pomodoro-start-sound (expand "bells.wav" emacs-assets-dir)
         org-pomodoro-finished-sound (expand "bells.wav" emacs-assets-dir)
         org-pomodoro-overtime-sound (expand "bells.wav" emacs-assets-dir)
         org-pomodoro-long-break-sound (expand "singing-bowl.wav" emacs-assets-dir)
         org-pomodoro-short-break-sound (expand "singing-bowl.wav" emacs-assets-dir)
         org-pomodoro-clock-break t
         org-pomodoro-manual-break t)

  (defun meditate-pomodoro ()
    (interactive)
    (csetq org-pomodoro-length 10)
    (org-pomodoro-start))

  (defun work-pomodoro ()
    (interactive)
    (csetq org-pomodoro-length 25
           org-pomodoro-short-break-length 5
           org-pomodoro-long-break-length 20)
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
  :config
  (map ju-notes-map
   "t" '(org-journal-open-current-journal-file :wk "Today's journal")
   "o" '(org-journal-new-entry :wk "Open journal"))

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
  (org-superstar-headline-bullets-list '("⁖" "•" "•" "•" "•" "•")) ;;  usechar ⁖`'  if first symbol don't work
  (org-superstar-item-bullet-alist '((?* . ?•) (?+ . ?▹) (?- . ?◦)))) ; changes +/- symbols in item lists

(use-package toc-org
  :config
  ;; enable in markdown, too
  (add-hook 'markdown-mode-hook 'toc-org-mode))
(defun tangle-reload ()
  (interactive)
  (let ((org-file (concat emacs-dir "cfg.org"))
        (init-file (concat emacs-dir "init.el")))
    (org-babel-tangle-file org-file) (load init-file)))

(use-package org-download
  :config
  (setq org-download-display-inline-images t
        org-download-image-dir (expand "_img" user-notes-dir))
  :gfhook
  ('dired-mode-hook 'org-download-enable))

;;; notes.el ends here
(use-package bug-hunter
  :ensure (:host github :repo "Malabarba/elisp-bug-hunter"))

(use-package org-roam
  :disabled t
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
(use-package denote
  :ensure t
  :init
  (require 'denote-org-extras)
  (denote-rename-buffer-mode t)
  :hook
  (find-file-hook . denote-link-buttonize-buffer)
  (dired-mode . denote-dired-mode)
  :custom-face
  (denote-faces-link ((t (:slant italic))))
  :custom
  (denote-known-keywords '("rust"
                           "emacs"
                           "CS"
                           "philosophy"
                           "politics"
                           "economics"))
  :config
  (defun my-denote-tmr ()
    (interactive)
    (tmr "10" "Practice writing in my journal"))
  (add-hook 'denote-journal-extras-hook 'my-denote-tmr)

  (csetq denote-directory user-notes-dir)
  (map ju-notes-map
       "n" #'denote-create-note
       "j" #'denote-journal-extras-new-or-existing-entry
       "d" #'denote-date
       "i" #'denote-link-or-create
       "l" #'denote-find-link
       "b" #'denote-find-backlink
       "D" #'denote-org-dblock-insert-links
       "r" #'denote-rename-file-using-front-matter
       "R" #'denote-rename-file
       "k" #'denote-keywords-add
       "K" #'denote-keywords-remove))
;;; Calendar
(map calendar-mode-map
     "n" #'calendar-forward-week
     "d" #'calendar-backward-week
     ;; forward:  f & t (t dvorak vim-like eq. j in qwerty)
     "t" #'calendar-forward-day
     "T" #'calendar-forward-month
     "f" #'calendar-forward-day
     "F" #'calendar-forward-month
     ;; backwards: b & h (h dvorak vim-like eq. k)
     "h" #'calendar-backward-day
     "H" #'calendar-backward-month
     "b" #'calendar-backward-day
     "B" #'calendar-backward-month)
;;; notes.el ---  desc  -*- lexical-binding: t; -*-
;;; Commentary:
;; https://www.reddit.com/r/emacs/comments/d7x7x8/finally_fixing_indentation_of_quoted_lists/
;; https://emacs.stackexchange.com/questions/10230/how-to-indent-keywords-aligned
;;; Code:
(require 'core-packages)
(require 'core-lib)
(require 'keymaps)

(use-package dogears
  :ensure t
  :init
  (dogears-mode 1))

;; Denote extensions
(use-package consult-notes
  :ensure t
  :after denote
  :commands (consult-notes
             consult-notes-search-in-all-notes)
  :init
  (csetq consult-notes-file-dir-sources (list `("All" 'a "~/notes")))
  (consult-notes-denote-mode)
  (map leader-map "C-n" #'consult-notes)
  (map ju-notes-map
       "f" #'consult-notes
       "s" #'consult-notes-search-in-all-notes))

(use-package org-sticky-header
  :ensure t
  :hook (org-mode . org-sticky-header-mode))

(use-package org-gtd
  :disabled t
  :ensure t
  :after org
  :init
  (setq ju-gtd-map (make-sparse-keymap))
  (map ju-notes-map
       "g" (cons "GTD" ju-gtd-map))

  (map ju-gtd-map
       "c" (cons "GTD: Capture" #'org-gtd-capture)
       "p" (cons "GTD: Process" #'org-gtd-process-inbox)
       "e" (cons "GTD: Engage" #'org-gtd-engage)
       "o" (cons "GTD: Organize" #'org-gtd-organize))

  (csetq org-gtd-update-ack "3.0.0"
         org-gtd-directory user-notes-dir))

;; Easy insertion of weblinks
(use-package org-web-tools)
(use-package ob-mermaid)

(use-package persistent-scratch
  :hook
  (after-init . persistent-scratch-setup-default)
  :init
  (persistent-scratch-autosave-mode)
  (csetq persistent-scratch-save-file (concat cache-dir "permanent-scratch.org")
         persistent-scratch-backup-directory (concat cache-dir "perm-scratch/"))
  :config
  (map ju-menu-map "x" #'scratch-buffer))

;; Modernise Org mode interface
(use-package olivetti
  :demand t)


(use-package org-bookmark-heading
  :ensure t)

(provide 'notes)
