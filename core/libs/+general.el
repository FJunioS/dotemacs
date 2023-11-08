;;; +general.el ---  desc  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defvar leader-key "SPC")
(defvar non-normal-leader-key "C-c")
(general-auto-unbind-keys)

(general-create-definer leader
  :keymaps '(normal visual emacs)
  :prefix leader-key
  :global-prefix non-normal-leader-key)

(defalias 'def #'general-def)
(defalias 'kbd! #'general-simulate-key)
(defalias 'gsetq #'general-setq)
(defalias 'gsetq-default #'general-setq-default)
(defalias 'gsetq-local #'general-setq-local)

(general-create-definer nmap :states 'normal)
(general-create-definer vmap :states 'visual)
(general-create-definer imap :states 'insert)
(general-create-definer mmap :states 'motion)
(general-create-definer nvmap :states '(normal visual))
(general-create-definer nvimap :states '(normal visual insert))
(general-create-definer global-key :states '(normal visual insert emacs) :keymaps 'global-map)
(general-create-definer global-set-key! :states '(normal visual insert emacs) :keymaps 'global-map)

(defun create-gen (key prefix-map prefix-cmd)
  (eval `(general-create-definer
     ;; Create procedures like `leader/file' or `leader/agenda'
     ;;to better handle custom commands with General
     ,(intern (format "leader/%s" (symbol-name prefix-cmd)))
     :states '(normal visual)
     :prefix-keymap ',prefix-map
     :prefix-command ',prefix-cmd
     ;; Only needed if using evil-mode
     :prefix ,(format "%s %s" leader-key key)
     :global-prefix ,(format "%s %s" non-normal-leader-key key))))

(defun define-leader-commands-from-list (command-list)
  "Get `custom-command-list' and create procedures and define maps on top of it."
  (let ((lst (seq-partition command-list 2)))
    (dolist (group lst)
      (let* ((label (car group))
             (prefix-cmd (cadr group))
             (key (substring (symbol-name prefix-cmd) 0 1))
             (prefix-map (intern (format "leader-%s-map" (symbol-name prefix-cmd)))))
        (create-gen key prefix-map prefix-cmd)
        (leader
          key '(:ignore t :wk label))))))

(setq custom-command-list
      '("Agenda"    agenda
        "Dired"     buffer
        "Code"      code
        "Emacs/eval"     emacs
        "Files"     file
        "Git/VCS"   vcs
        "Help"      help
        "Major-Mode" mode
        "Notes/Org" notes
        "Open"      open
        "Project"   project
        "register"  register
        "Search"    search
        "Toggle"    toggle
        "Workspace" workspace))

(define-leader-commands-from-list custom-command-list)

(general-create-definer leader/system
  :states '(normal visual)
  :prefix-keymap 'leader-system-map
  :prefix-command 'system
  ;; Only needed if using evil-mode
  :prefix (format "%s <tab>" leader-key)
  :global-prefix (format "%s <tab>" non-normal-leader-key)
  "" '(:ignore t :wk "System"))

(nvmap
  :prefix-map 'leader-system-map
  :prefix-command 'system
  :prefix "s")

(nvmap
  :prefix-map 'leader-file-map
  :prefix-command 'file
  :prefix "f")

(nvmap
  :prefix-map 'leader-mode-map
  :prefix-command 'mode
  :prefix "m")

(provide '+general)

;;; +general.el ends here
