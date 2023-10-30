;;; my-emacs-keymaps.el --- Where all keymaps are defined
;;
;;; Commentary:
;; Plugins
;;
;;; Code:
(defvar my-leader-keys ())
(use-package general
	:config
	(general-create-definer my-leader-keys
		:keymaps '(emacs normal)
		:prefix "SPC"
		:non-normal-prefix "C-c"))

(use-package multiple-cursors)
(use-package which-key
	:defer 0
	:custom
	(which-key-show-early-on-C-h t)
	(which-key-idle-delay 1)
	(which-key-idle-secondary-delay 0.01)
	(which-key-unicode-correction 3)
	(which-key-show-remaining-keys t)
	(which-key-max-display-columns 5)

	(which-key-add-column-padding 10)
	(which-key-sort-order 'which-key-key-order-alpha)
	:config
	(which-key-mode)
	(which-key-setup-side-window-bottom)
	nil)

(use-package undo-tree
	:config
	(setq evil-want-keybinding nil)
	(setq undo-tree-history-directory-alist
		`((".*" . ,temporary-file-directory)))
	(global-undo-tree-mode 1))

;; lets get used to emacs way
(require 'my-evil)

(require 'vertico)
(map! vertico-map
      "C-k" vertico-previous
      "C-j" vertico-next
      "C-l" jun--insert-and-complete
      "ESC" vertico-exit-input
      "TAB" vertico-insert
      "C-u" vertico-scroll-up
      "C-d" vertico-scroll-down
      "C-r" consult-history)

;;-----------------------------------------------
;; Keymaps definition:
;;
;; `my-leader-keys' definition :: C-c
(my-leader-keys
	"" nil
	"/" 'consult-line
	"SPC" 'consult-buffer
	"." 'project-find-file
	"," 'projectile-recentf

	"C-j" '(org-journal-new-entry :wk "New entry :[Org Journal]")
	"C-o" '(kill-other-buffers :wk "Kill all buffers but current")
	
	"a" '(:ignore t :wk "Agenda")
	"a a" '(consult-org-agenda :wk "Open")
	"a c" '(calendar :wk "Calendar")

	"c" '(:ignore t :wk "code")
	"c n" '(lambda () (interactive) (cd "~/docs/") (call-interactively 'find-file))

	"f" '(:ignore t :wk "File")
	"c p" '(lambda () (interactive) (cd user-emacs-sync-directory)(call-interactively 'find-file))
	"f r" 'consult-recent-file
	"f f" 'find-file

	"b" '(:ignore t :wk "Buffer")
	"b k" 'kill-this-buffer
	"b n" 'next-buffer
	"b p" 'previous-buffer
	"b r" 'revert-buffer
	"b i" 'ibuffer
	"b b" 'switch-to-buffer

	"e" '(:ignore t :wk "Evaluate")    
	"e b" 'eval-buffer
	"e d" 'eval-defun
	"e e" 'eval-expression
	"e l" 'eval-last-sexp
	"e r" 'eval-region

	"g" '(:ignore t :wk "Magit")
	"g g" 'magit
	"g p" 'magit-push
	"g P" 'magit-pull
	"g l" 'magit-clone
	"g c" 'magit-commit
	"g s" 'magit-status
	"g t" 'magit-tag
	"g S" 'magit-stash
	"g C" 'magit-cherry
	"g i" 'magit-gitignore
	"g f" 'magit-fetch
	"g w" 'magit-worktree

	"n" '(:ignore t :wk "Org")
	"n c" '(org-roam-capture :wk "[C]apture") 
	"n d" '(deft :wk "Deft Find")
	"n f" '(org-roam-node-find :wk "[F]ind Node")
	"n t" '(org-journal-open-current-journal-file :wk "Journal [T]oday's entry")
	"n o" '(org-journal-new-entry :wk "Open Journal entry")

	"h" '(:ignore t :wk "Help")
	"h a" 'apropos-command
	"h c" 'helpful-callable
	"h f" 'helpful-function
	"h C" 'helpful-command
	"h k" '(helpful-key :wk "descript Key") 
	"h m" '(helpful-macro :wk "[M]acros")
	"h p" '(helpful-at-point :wk "help at [P]oint")
	"h o" '(helpful-symbol :wk "[S]ymbol"))

;; `consult.el' specific 
(global-set-key (kbd "C-x C-r") 'consult-recent-file)
(global-set-key (kbd "C-M-j") 'consult-buffer)
(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "M-y") 'consult-yank-from-kill-ring)

;; `embark.el'
(global-set-key (kbd "C-.") 'embark-act)

(require 'corfu)
(require 'corfu-quick)
(map! corfu-map
      "ESC" corfu-quit
      "C-SPC" corfu-insert-separator 
      "#" corfu-insert-separator
      "C-j" corfu-complete
      "C-v" corfu-scroll-down
      "C-V" corfu-scroll-up
      "SPC" +!jun/quit_and_jump
      "TAB" corfu-insert)

(dolist (c (list (cons "SPC" " ")
			   (cons "." ".")
               (cons "," ",")
               (cons ":" ":")
               (cons ")" ")")
               (cons "}" "}")
               (cons "]" "]")))
	(define-key corfu-map (kbd (car c)) `(lambda ()
                                           (interactive)
                                           (corfu-quit)
                                           (insert ,(cdr c)))))

;; `helpful.el'
(global-set-key (kbd "C-h .") 'helpful-at-point)

;; `org'
;; enable todo things
(define-key org-mode-map (kbd "C-c C-d") 'org-toggle-todo-and-fold)
(global-set-key [C-tab] 'completion-at-point)

(my-leader-keys
	"w" '(:ignore t :wk "Workspace")
	"w a" '(persp-add-buffer :wk "Add buffer")
	"w l" '(persp-switch-last :wk "Go to last workspace")
	"w n" '(persp-next :wk "next workspace")
	"w p" '(persp-prev :wk "previous workspace")
	"w r" '(persp-rename :wk "Rename current workspace")
	"w w" '(persp-switch :wk "Switch")

	"w K" '(persp-kill :wk "Kill current buffer")
	"w O" '(persp-kill-others :wk "Kills all others workspaces")

	"1" '((lambda () (interactive) (persp-switch-by-number 1)))
	"2" '((lambda () (interactive) (persp-switch-by-number 2)))
	"3" '((lambda () (interactive) (persp-switch-by-number 3)))
	"4" '((lambda () (interactive) (persp-switch-by-number 4)))
	"5" '((lambda () (interactive) (persp-switch-by-number 5)))
	)

(provide 'my-emacs-keymaps)
;;; my-emacs-keymaps.el ends here
