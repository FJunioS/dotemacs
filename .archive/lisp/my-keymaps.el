
;; (defun jun--insert-and-complete ()
;;   "Select current candidate on vertico mode"
;;   (interactive)
;;    (progn
;;      (vertico-insert)

(defun newline-without-break-of-line ()
  "1. move to end of the line.
  2. insert newline with index"
  (interactive)
  (let ((oldpos (point)))
    (end-of-line)
    (newline-and-indent)))

(defun newline-upward-without-break-of-line ()
  (interactive)
  (previous-line)
  (end-of-line)
  (newline-and-indent))

(defun my-switch-to-previous-buffer ()
  "Switch to previously open buffer.
			Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun my-delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun my-backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
  (interactive "p")
  (my-delete-word (- arg)))

(defun my-delete-line ()
  "Delete text from current position to end of line char.
This command does not push text to `kill-ring'."
  (interactive)
  (delete-region
   (point)
   (progn (end-of-line 1) (point)))
  (delete-char 1))

(defun mark-whole-word (&optional arg allow-extend)
  "Like `mark-word', but selects whole words and skips over whitespace.
		     If you use a negative prefix arg then select words backward.
		     Otherwise select them forward.

		     If cursor starts in the middle of word then select that whole word.

		     If there is whitespace between the initial cursor position and the
		     first word (in the selection direction), it is skipped (not selected).

		     If the command is repeated or the mark is active, select the next NUM
		     words, where NUM is the numeric prefix argument.  (Negative NUM
		     selects backward.)"
  (interactive)
  (let ((num  (prefix-numeric-value arg)))
    (unless (eq last-command this-command)
      (if (natnump num)
	  (skip-syntax-forward "\\s-")
	(skip-syntax-backward "\\s-")))
    (unless (or (eq last-command this-command)
		(if (natnump num)
		    (looking-at "\\b")
		  (looking-back "\\b")))
      (if (natnump num)
	  (left-word)
	(right-word)))
    (mark-word arg allow-extend)))

(defun my-delete-line-backward ()
  "Delete text between the beginning of the line to the cursor position.
This command does not push text to `kill-ring'."
  (interactive)
  (let (p1 p2)
    (setq p1 (point))
    (beginning-of-line 1)
    (setq p2 (point))
    (delete-region p1 p2)))

(defun org-toggle-todo-and-fold ()
  (interactive)
  (save-excursion
    (org-back-to-heading t) ;; Make sure command works even if point is
                            ;; below target heading
    (cond ((looking-at "\*+ TODO")
           (org-todo "DONE")
           (hide-subtree))
          ((looking-at "\*+ DONE")
           (org-todo "TODO")
           (hide-subtree))
          (t (message "Can only toggle between TODO and DONE.")))))

