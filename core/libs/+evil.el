

;;; +evil.el ---  desc  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'core-lib)
(require 'cl-lib)

(use-package evil
  :defer 0
  :diminish evil-mode
  :init
  (setq evil-move-cursor-back nil
         evil-move-beyond-eol nil
         evil-want-C-i-jump t
         evil-want-fine-undo t
         evil-want-C-u-scroll t
         evil-want-Y-yank-to-eol t
         ;; default to inserting `<,`> when run `evil-ex' in visual
         ;; char state; unlike vim, ex commands will only apply to
         ;; the selected region instead of the selected lines when
         ;; `<,`> is used
         evil-ex-visual-char-range t
         ;; Required
         evil-want-keybinding nil
         evil-want-integration nil
         evil-insert-state-bindings nil)

  :config
  (evil-mode 1)
  (with-eval-after-load 'general
    (general-remove-advice 'require #'prevent-evil-requires)
    ;; Unset behaviors for future enhancement
    (general-def 'normal
      "q" nil
      "m" nil)

    (general-def 'motion
      "k" #'evil-previous-visual-line
      "j" #'evil-next-visual-line))

  (add-hook 'with-editor-mode-hook 'evil-insert-state)
  (evil-set-initial-state 'magit-status-mode 'normal)

  (setq evil-normal-state-modes
         (append evil-emacs-state-modes
                 evil-normal-state-modes)
         evil-emacs-state-modes nil
         evil-motion-state-modes nil))

(use-package evil-commentary
  :config (evil-commentary-mode))

(use-package evil-surround)

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode))

(use-package evil-visualstar
  :config (global-evil-visualstar-mode))

(use-package evil-visual-mark-mode
  :defer 3
  :config
  (evil-visual-mark-mode))

(use-package evil-indent-textobject)

(use-package evil-args
  :config
  ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

;; ** PARENTHESIS HOTFIX
(defun show-paren--locate-near-paren-ad ()
  "Locate an unescaped paren \"near\" point to show.
If one is found, return the cons (DIR . OUTSIDE), where DIR is 1
for an open paren, -1 for a close paren, and OUTSIDE is the buffer
position of the outside of the paren.  Otherwise return nil."
  (let* ((before (show-paren--categorize-paren (point))))
    (when (or
       (eq (car before) 1)
       (eq (car before) -1))
      before)))

(advice-add 'show-paren--locate-near-paren :override #'show-paren--locate-near-paren-ad)

(defun evil-adjust-eval-print-last-sexp (&optional arg)
  "Evaluate the sexp before point and print it on a new line.

This function is a wrapper around `eval-print-last-sexp' which corrects for
cursor position in normal/visual states when `evil-move-cursor-back' is set to
`t' (as by default).

Long output is truncated. See the variables `eval-expression-print-length' and
`eval-expression-print-level'.
A prefix argument of 0 inhibits truncation and prints integers with additional
octal, hexadecimal and character representations, in the format: 1 (#o1, #x1,
?\C-a).

Errors start the debugger unless an argument of `nil' is passed for
`eval-expression-debug-on-error'."
  (interactive "P")
  (cl-case evil-state
    ('normal (progn
               (evil-append 1)
               (eval-print-last-sexp -1)
               (evil-normal-state)
               ))
    ('visual (progn
               (evil-append 1)
               (eval-print-last-sexp -1)
               (evil-visual-restore)
               ))
    (otherwise (eval-print-last-sexp -1))
    ))

(defun evil-adjust-eval-last-sexp (&optional arg)
  "Evaluate the sexp before point and print it in the echo area.

This function is a wrapper around `eval-last-sexp' which corrects for cursor
position in normal/visual states when `evil-move-cursor-back' is set to `t'
(as by default).

Long output is truncated. See the variables `eval-expression-print-length' and
`eval-expression-print-level'.
A prefix argument of 0 inhibits t
runcation and prints integers with additional
octal, hexadecimal and character representations, in the format: 1 (#o1, #x1,
?\C-a).

Errors start the debugger unless an argument of `nil' is passed for
`eval-expression-debug-on-error'."
  (interactive "P")
  (cl-case evil-state
    ('normal (progn
               (if (memq major-mode '(org-mode))
                   (evil-org-append-line 1)
                 (evil-append 1))
               (call-interactively #'eval-last-sexp)
               (evil-normal-state)
               ))
    ('visual (progn
               (if (memq major-mode '(org-mode))
                   (evil-org-append-line 1)
                 (evil-append 1))
               (call-interactively #'eval-last-sexp)
               (evil-visual-restore)
               ))
    (otherwise (eval-last-sexp))
    ))

(with-eval-after-load 'general
(general-def 'normal :keymaps +lisp-modes
  [remap eval-last-sexp] 'evil-adjust-eval-last-sexp
  [remap eval-print-last-sexp] 'evil-adjust-eval-print-last-sexp)

(general-def 'visual :keymaps +lisp-modes
  [remap eval-last-sexp] #'(lambda ()
                             (interactive)
                             (call-interactively #'eval-region)
                             (evil-normal-state 1)
                             (message "Region evaluated"))))

;; Cycle through without passing through Messages or scratch buffers
(provide '+evil)
;;; +evil.el ends here
