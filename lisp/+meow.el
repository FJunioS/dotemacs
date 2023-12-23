;;; $EMACS_DIR/lisp/+meow.el --  -*- lexical-binding: t -*-
;;; Commentary:
;; Settings to define `meow.el' commands and keymaps;
;;; Code:

(defun meow-setup ()
  "Meow keymap definitions."
  (require 'meow nil t)

  (csetq meow-keypad-leader-dispatch leader-map
         meow-expand-hint-counts 0)

  ;; Remove Meta and C-M from SPC keybind
  (csetq-default meow-keypad-meta-prefix nil
                 meow-keypad-ctrl-meta-prefix nil)

  ;; Leader (SPC/C-c)
  (meow-leader-define-key
   '("<tab>" . mode-line-other-buffer)
   '("m" . "C-c m")
   '("g" . "C-c g")
   '("?" . meow-cheatsheet)
   '("n" . ju-notes-map))

  ;; Motion
  (meow-motion-overwrite-define-key
   ;; custom keybinding for motion state
   '("<escape>" . escape))

  ;; Insert
  (map meow-insert-state-keymap
       "C-SPC" #'completion-at-point)

  (defun ju-meow-change--handle-region (&optional arg)
    "Kill region or copy if ARG."
    (interactive "r")
    (let* ((region (region-bounds))
           (beg (car region))
           (end (cdr region)))
      (if arg
          (kill-ring-save nil nil t)
        (kill-region nil nil t)
        (meow-insert))))

  (defun ju-meow-change (&optional arg)
    "when no selection replace word on cursor, if active region, kill region and enter insert mode."
    (interactive "P")
    (if (region-active-p)
        (ju-meow-change--handle-region arg)
      ;;else
      (when arg (delete-char 1))
      (meow-insert)))

  (defun ju-meow-change-dwim (&optional arg)
    "when no selection replace word on cursor, if active region, kill region and enter insert mode."
    (interactive "P")
    (unless (region-active-p)
      (let* ((bds (bounds-of-thing-at-point 'symbol))
             (beg (car bds))
             (end (cdr bds)))
        (cond (arg (kill-ring-save beg end))
              (t   (kill-region beg end)))
        (meow-insert))))

  (defun ju-append ()
    (interactive)
    (back-to-indentation)
    (meow-insert))

  (defun ju-insert ()
    (interactive)
    (goto-char (eol))
    (meow-insert))

  ;; Normal
  (meow-normal-define-key
   '("C-d" . ju-delete-char)
   '("C-u" . universal-argument)

   '("u" . undo)
   '("U" . undo-redo)
   '("#" . popper-toggle)
   '("." . embark-act)
   '("," . execute-extended-command)
   '(";" . repeat)
   '("]" . ace-window)

   '("a" . mwim-beginning-of-code-or-line)
   '("A" . ju-append)
   '("c" . ju-meow-change)
   '("C" . ju-meow-change-dwim)
   '("d" . ju-delete-char)
   '("e" . mwim-end-of-code-or-line)
   '("h" . ju-menu-map)
   '("b" . meow-left)
   '("B" . meow-left-expand)
   '("f" . avy-goto-char-2)
   '("i" . ju-insert)
   '("I" . ju-insert)
   '("k" . kill-visual-line-join)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-open-below-visual)
   '("O" . meow-open-above-visual)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)
   '("t" . meow-right)
   '("T" . meow-right-expand)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("w" . meow-back-word)
   '("W" . meow-back-symbol)
   '("v" . meow-next-word)
   '("V" . meow-next-symbol)
   '("l" . meow-line)
   '("q" . ignore)
   )
  )

(provide '+meow)
;;; +meow.el ends here
