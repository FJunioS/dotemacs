;;; +general.el ---  desc  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

    (general-create-definer nmap :states 'normal)
    (general-create-definer vmap :states 'visual)
    (general-create-definer imap :states 'insert)
    (general-create-definer mmap :states 'motion)
    (general-create-definer nvmap :states '(normal visual))
    (general-create-definer nvimap :states '(normal visual insert))
    (general-create-definer global-key :states '(normal visual insert emacs) :keymaps 'global-map)

    (defmacro create-gen (key prefix)
      `(general-create-definer
         ;; Create procedures like `leader/file' or `leader/agenda'
         ;;to better handle custom commands with General
         ,(intern (replace-regexp-in-string "\'" "" (format "leader/%s" prefix)))
         :states '(normal visual)
         ;; Only needed if using evil-mode
         :prefix ,(format "%s %s" leader-key key)
         ))

(defun define-leader-commands-from-list (command-list)
  "Get `custom-command-list' and create procedures and define maps on top of it."
  `(progn
     ,@(seq-map (lambda (group)
                  (let ((key (nth 0 group))
                        (label (nth 1 group))
                        (prefix-map (nth 2 group))
                        (prefix-cmd (nth 3 group)))
                    `(progn
                       (general-def :states 'normal :prefix-command ,prefix-cmd :prefix-map ,prefix-map :keymaps 'global-map)
                       (create-gen ,key ,prefix-cmd)
                       (leader :infix ,key :prefix-command ,prefix-cmd)
                       (leader ,key '(:ignore t :wk ,label)))))
                (seq-partition command-list 4))))

(setq custom-command-list
      '("a" "Agenda" 'leader-agenda-map 'agenda
        "b" "Dired" 'leader-buffer-map 'buffer
        "c" "Code" 'leader-code-map 'code
        "d" "Dired" 'leader-dired-map 'dired
        "e" "Eval" 'leader-eval-map 'eval
        "f" "File" 'leader-file-map 'file
        "g" "Git/VCS" 'leader-vcs-map 'vcs
        "h" "Help" 'leader-help-map 'help
        "m" "Major Mode" 'leader-mode-map 'mode
        "n" "Notes/Org" 'leader-notes-map 'notes
        "o" "Open" 'leader-open-map 'open
        "p" "Project" 'leader-project-map 'project
        "s" "system" 'leader-system-map 'system
        "t" "Toggle" 'leader-toggle-map 'toggle
        "w" "Workspace" 'leader-workspace-map 'workspace
        "y" "Yas!" 'leader-yasnippet-map 'yasnippet))

;; Eval is needed (don't ask me why)
(eval (define-leader-commands-from-list custom-command-list))

(nvmap
  :prefix-map leader-system-map
  :prefix-command 'system
  :prefix "s")

(nvmap
  :prefix-map leader-file-map
  :prefix-command 'file
  :prefix "f")

(nvmap
  :prefix-map leader-mode-map
  :prefix-command 'mode
  :prefix "m")

(provide '+general)

;;; +general.el ends here
