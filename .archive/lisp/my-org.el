;;; my-org.el --- Set of customizations to enhance org workflow
(use-package all-the-icons)
(use-package org
  :straight (:type built-in)
  :hook
  (org-mode . turn-on-auto-fill)
  (org-mode . (lambda () (setq fill-column 80)))
  :custom
;;; Prettify things
  (org-pretty-entities t)
  (org-src-fontify-natively t) ;; Use font config on source blocks
  (org-hide-emphasis-markers t) ;; Hide *, /, =, on chars
  (org-startup-indented t) ;; Be able to recognize things at first
  (org-adapt-indentation nil) ;; Personally don't like the results
  (org-startup-folded t) ;; Be able to find things at first
  (org-catch-invisible-edits nil) ;; block (confusing) invisible edits ("...")

;;; Logs
  (org-agenda-start-with-log-mode t)
  (org-log-done 'time)
  (org-log-into-drawer t)

;;; Images on org
  (org-habit-graph-column 60)
  (org-startup-with-inline-images t)
  (org-image-actual-width '(300))

  (org-list-indent-offset 2)  ;; how much to indent subitens

  (org-tab-first-hook #'+org-cycle-only-current-subtree-h)  ;; Tab (un)folds subtrees instead of cycling

;;; ToDo
  (org-enforce-todo-dependencies t)  ;; Just complete todo if sub-itens are also completed
  (org-todo-keyword-faces
   '(("TODO" :foreground "red" :weight bold)
     ("NEXT" :foreground "blue" :weight bold)
     ("DONE" :foreground "forest green" :weight bold)
     ("WAITING" :foreground "orange" :weight bold)
     ("HOLD" :foreground "magenta" :weight bold)
     ("CANCELLED" :foreground "forest green" :weight bold)
     ("MEETING" :foreground "forest green" :weight bold)
     ("PHONE" :foreground "forest green" :weight bold)))

;;; Org Agenda
  (org-cycle-separator-lines 2)
  (org-agenda-breadcrumbs-separator " ❱ ")
  (org-agenda-current-time-string "⏰ ┈┈┈┈┈┈┈┈┈┈┈ now")
  (org-agenda-skip-scheduled-if-done t)
  (org-agenda-skip-deadline-if-done t)
  (org-agenda-include-deadlines t)
  (org-agenda-block-separator #x2501)
  (org-agenda-compact-blocks t)
  (org-agenda-start-with-log-mode t)
  (org-agenda-category-icon-alist
   `(("Work" ,(list (all-the-icons-faicon "cogs")) nil nil :ascent center)
     ("Personal" ,(list (all-the-icons-material "person")) nil nil :ascent center)
     ("Calendar" ,(list (all-the-icons-faicon "calendar")) nil nil :ascent center)
     ("Reading" ,(list (all-the-icons-faicon "book")) nil nil :ascent center)))
  (org-agenda-deadline-faces '((1.0001 . org-warning)              ; due yesterday or before
			       (0.0    . org-upcoming-deadline)))  ; due today or later
  (org-agenda-clockreport-parameter-plist
   '(:link t :maxlevel 5 :fileskip0 t :compact t :narrow 80))
  (org-agenda-files (list "~/sync/org/journal/"
			  "~/sync/org/work.org"
			  "~/sync/org/dev.org"))
  (org-agenda-time-grid '((weekly today require-timed)
                          (800 1000 1200 1400 1600 1800 2000)
                          "---" "┈┈┈┈┈┈┈┈┈┈┈┈┈"))
  (org-agenda-prefix-format '((agenda . "%i %-12:c%?-12t%b% s")
                              (todo . " %i %-12:c")
                              (tags . " %i %-12:c")
                              (search . " %i %-12:c")))

;;; Org Clock
  (org-clock-into-drawer t)
  (org-clock-history-length 23) ;; Show lot of clocking history so it's easy to pick items off the C-F11 list
  (org-clock-in-resume t) ;; Resume clocking task on clock-in if the clock is open
  (org-clock-out-remove-zero-time-clocks t) ;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
  (org-clock-out-when-done t) ;; Clock out when moving task to a done state
  (org-clock-persist t) ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (org-clock-report-include-clocking-task t) ;; Include current clocking task in clock reports

;;; Org Refile
  (org-refile-targets '((org-agenda-files . (:maxlevel . 6))))
  (org-refile-use-outline-path 'file)
  (org-refile-allow-creating-parent-nodes 'confirm)

  :config
  (org-clock-persistence-insinuate) ;; Resume clocking task when emacs is restarted
  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (add-hook 'org-mode-hook 'visual-line-mode))

;; Nice bullets
(use-package org-superstar
  :custom
  (set-face-attribute 'org-superstar-item nil :height 0.8)
  (set-face-attribute 'org-superstar-header-bullet nil :height 0.8)
  (set-face-attribute 'org-superstar-leading nil :height 0.8)
  (org-ellipsis " ⋯")
  (org-superstar-special-todo-items t)
  (org-superstar-headline-bullets-list '("" "•" "•" "•" "•" "•")) ;; ⁖  if 1. dont work
  (org-superstar-item-bullet-alist '((?* . ?•) (?+ . ?▹) (?- . ?◦))) ; changes +/- symbols in item lists
  
  (org-superstar-todo-bullet-alist
   ;; Enable custom bullets for TODO items
   '(("TODO" . ?☐)
     ("NEXT" . ?✒)
     ("HOLD" . ?✰)
     ("WAITING" . ?☕)
     ("CANCELLED" . ?✘)
     ("DONE" . ?✔)))
  :config
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))

;; Table of content
(use-package toc-org) 

(require 'org-habit)
(add-to-list 'org-modules 'org-habit)

(setq org-todo-keywords
    (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
            (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

;; Save Org buffers after refiling!
(advice-add 'org-refile :after 'org-save-all-org-buffers)
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

;; Configure custom agenda views
(setq org-agenda-custom-commands
      '(("d" "Dashboard"
         ((agenda "" ((org-deadline-warning-days 7)))
          (todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))
          (tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

        ("n" "Next Tasks"
         ((todo "NEXT"
                ((org-agenda-overriding-header "Next Tasks")))))

        ("W" "Work Tasks" tags-todo "+work-email")

        ;; Low-effort next actions
        ("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
         ((org-agenda-overriding-header "Low Effort Tasks")
          (org-agenda-max-todos 20)
          (org-agenda-files org-agenda-files)))

        ("w" "Workflow Status"
         ((todo "WAIT"
                ((org-agenda-overriding-header "Waiting on External")
                 (org-agenda-files org-agenda-files)))
          (todo "REVIEW"
                ((org-agenda-overriding-header "In Review")
                 (org-agenda-files org-agenda-files)))
          (todo "PLAN"
                ((org-agenda-overriding-header "In Planning")
                 (org-agenda-todo-list-sublevels nil)
                 (org-agenda-files org-agenda-files)))
          (todo "BACKLOG"
                ((org-agenda-overriding-header "Project Backlog")
                 (org-agenda-todo-list-sublevels nil)
                 (org-agenda-files org-agenda-files)))
          (todo "READY"
                ((org-agenda-overriding-header "Ready for Work")
                 (org-agenda-files org-agenda-files)))
          (todo "ACTIVE"
                ((org-agenda-overriding-header "Active Projects")
                 (org-agenda-files org-agenda-files)))
          (todo "COMPLETED"
                ((org-agenda-overriding-header "Completed Projects")
                 (org-agenda-files org-agenda-files)))
          (todo "CANC"
                ((org-agenda-overriding-header "Cancelled Projects")
                 (org-agenda-files org-agenda-files)))))))

;; Credit to Hugo Cisneros::https://hugocisneros.com/org-config
(setq org-capture-templates
      '(("n" "Notes" entry
         (file "~/sync/org/inbox.org") "* %^{Description} %^g\n Added: %U\n%?")
        ("m" "Meeting notes" entry
         (file "~/sync/org/meetings.org") "* TODO %^{Title} %t\n- %?")
        ("t" "TODO" entry
         (file "~/sync/org/inbox.org") "* TODO %^{Title}")
        ("e" "Event" entry
         (file "~/sync/org/calendar.org") "* %^{Is it a todo?||TODO |NEXT }%^{Title}\n%^t\n%?")
        ("w" "Work TODO" entry
         (file "~/sync/org/work.org") "* TODO %^{Title}")))

(push '("conf-unix" . conf-unix) org-src-lang-modes)

(require 'org-tempo)
(add-to-list 'org-structure-template-alist '("sh" . "src shell"))
(add-to-list 'org-structure-template-alist '("r" . "src rust"))
(add-to-list 'org-structure-template-alist '("el" . "src elisp"))
(add-to-list 'org-structure-template-alist '("py" . "src python"))
;;

;; Create flashcards
(use-package org-drill)

;; Second-brain
(use-package org-roam
  :ensure t
  :custom
  (org-id-link-to-org-use-id t)
  (org-roam-completion-system 'vertico)
  (org-roam-directory (file-truename "~/sync/org/notes/"))
  (org-roam-completion-everywhere t)
  (org-roam-db-gc-threshold most-positive-fixnum)
  (org-roam-mode-sections
   (list #'org-roam-backlinks-section
	 #'org-roam-reflinks-section
	 #'org-roam-unlinked-references-section))
  (org-roam-capture-templates
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
  (org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  :config
  (add-to-list 'display-buffer-alist
	       '("\\*org-roam\\*"
		 (display-buffer-in-direction)
		 (direction . right)
		 (window-width . 0.33)
		 (window-parameters . ((no-other-window . t)
				       (no-delete-other-windows . t)))))
  (setq org-id-extra-files (org-roam--list-files org-roam-directory))
  (org-roam-setup)
  (org-roam-db-autosync-mode))

;; Add timestamp on capture notes
(add-hook 'before-save-hook 'time-stamp)

;; Journaling
(use-package org-journal
  :custom
  (org-journal-date-prefix "#+title: ")
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/sync/org/journal/")
  (org-journal-date-format "%A, %d %B %Y"))

(add-hook 'org-journal-mode-hook
          (lambda ()
            (define-key org-journal-mode-map
			(kbd "C-x C-s") 'org-journal-save-entry-and-exit))) ;; Close Journal on save

;; Org Ref and Biblex
(setq org-cite-global-bibliography nil)
(add-to-list 'org-cite-global-bibliography (expand-file-name "~/docs/library.json"))
(require 'oc-csl)
(with-eval-after-load 'org-ref
  (setq bibtex-completion-bibliography '("~/docs/library.json")
        bibtex-completion-library-path '("~/docs/pdf/")
        bibtex-completion-notes-path "~/sync/org/notes"
        bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-display-formats
        '((article       . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${journal:40}")
          (inbook        . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} Chapter ${chapter:32}")
          (incollection  . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (inproceedings . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*} ${booktitle:40}")
          (t             . "${=has-pdf=:1}${=has-note=:1} ${year:4} ${author:36} ${title:*}"))
        bibtex-completion-pdf-open-function
        (lambda (fpath)
          (call-process "open" nil 0 nil fpath)))
  ;; (require 'org-ref-helm)
  (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)
  (setq org-ref-insert-link-function 'org-ref-insert-link-hydra/body
        ;; org-ref-insert-cite-function 'org-ref-cite-insert-helm
        org-ref-insert-label-function 'org-ref-insert-label-link
        org-ref-insert-ref-function 'org-ref-insert-ref-link
        org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body)))
  )

;; Bibtex setup
(setq bibtex-completion-pdf-open-function
      (lambda (fpath)
        (cond ((eq system-type 'darwin) (start-process "open" "*open*" "open" fpath))
              ((eq system-type 'gnu/linux) (start-process "evince" "*evince*" "evince" fpath)))))
(setq bibtex-completion-pdf-field "file")
(setq bibtex-completion-pdf-symbol "⌘")
(setq bibtex-completion-notes-symbol "✎")
(setq bibtex-completion-notes-template-multiple-files
      ":PROPERTIES:\n:ROAM_REFS: cite:${=key=}\n:END:\n#+TITLE: Notes on: ${title} by ${author-or-editor} (${year})\n#+lastmod: Time-stamp: <>\n#+ROAM_KEY: cite:${=key=}\n\n- source :: cite:${=key=}
  \n\n* TODO Summary\n* TODO Comments\n\n
  bibliography:~/docs/library.json")

;;; Deft :: Interface for browsing org-roam files
(use-package deft
  :after org
  :custom
  (deft-recursive t)
  (deft-use-filter-string-for-filename t)
  (deft-default-extension "org")
  (deft-directory org-roam-directory))

;; Convert clips into LaTeX equations
(use-package mathpix.el
  :straight (:host github :repo "jethrokuan/mathpix.el")
  :custom ((mathpix-app-id "app-id")
           (mathpix-app-key "app-key"))
  :bind
  ("C-x m" . mathpix-screenshot))

;; Enable notes on PDF, Epub, etc.
(setq doc-view-resolution 300)
(use-package org-noter)
(use-package nov
  :init
	(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(provide 'my-org)
;;; my-org.el ends here
 
