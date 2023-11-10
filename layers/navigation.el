;;; navigation.el ---  desc  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'core-packages)
(require 'core-lib)

(general-def woman-node-map
  "]]" 'WoMan-next-manpage
  "[[" 'WoMan-previous-manpage
  "r" 'woman-reformat-last-file) ; refresh

(define-key emacs-lisp-mode-map (kbd "C-c C-k") #'helpful-at-point)
(define-key prog-mode-map (kbd "C-c C-k") #'eldoc-box-help-at-point)

(general-def
  "C-h =" #'describe-char
  "M-k" #'kill-this-buffer
  "M-o" #'evil-switch-to-windows-last-buffer)

(general-def :prefix "C-c"
  "-" #'evil-window-split
  "v" #'evil-window-vsplit)

(general-def :prefix "C-c f"
  "f" #'find-file
  "r" #'consult-recent-file
  "d" #'manual-save-buffer
  "o" '(:ignore t :wk "options")
  "p" #'(lambda () (interactive) (consult-fd "~/.emacs.d/")))

(global-set-key (kbd "C-c tw") #'toggle-word-wrap)

(use-package drag-stuff
  :init
  (drag-stuff-global-mode)
  :config
  (drag-stuff-define-keys)
  :general
  ("M-K" #'drag-stuff-up
   "M-J" #'drag-stuff-down))

(use-package multiple-cursors)

(use-package mwim
  :defer t
  :general
  ("C-a" #'mwim-beginning-of-code-or-line
    "C-e" #'mwim-end-of-code-or-line))

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(defmacro wrap-region-with-symbol! (sym &optional ending)
  `(defun ,(intern (format "wrap-with-%s"
                           (if (stringp sym)
                               sym
                             (cl-gensym)))) ()
     ,(format "Insert %s on beginning and end between mark" sym)
     (interactive)
     (let ((pos1 (region-beginning))
           (pos2 (region-end)))
       (setq mark-active nil)
       (save-excursion
         (goto-char pos1)
         (insert ,sym)
         (goto-char (1+ pos2))
         (if (stringp ,ending)
             (insert ,ending)
           (insert ,sym)))
       (forward-char)) ; Cursor stay on top of symbol
     ))

(general-def 'visual
  "C-b" (wrap-region-with-symbol! "*")
  "\"" (wrap-region-with-symbol! "\"")
  "_" (wrap-region-with-symbol! "_")
  "'" (wrap-region-with-symbol! "'")
  "`" (wrap-region-with-symbol! "`" "'")
  "(" (wrap-region-with-symbol! "(" ")")
  "[" (wrap-region-with-symbol! "[" "]")
  "{" (wrap-region-with-symbol! "{" "}"))

;;; Xah Lee Keymaps:
(defvar xah-brackets '("()" "[]" "{}" "<>")
  "A list of strings, each element is a string of 2 chars, the left
  bracket and a matching right bracket.
  Used by `xah-select-text-in-quote' and others.
  Version 2023-07-31")

(defconst xah-left-brackets
  (mapcar (lambda (x) (substring x 0 1)) xah-brackets)
  "List of left bracket chars. Each element is a string.")

(defconst xah-right-brackets
  (mapcar (lambda (x) (substring x 1 2)) xah-brackets)
  "List of right bracket chars. Each element is a string.")

(defun xah-select-text-in-quote ()
  "Select text between the nearest left and right delimiters.
  Delimiters here includes the following chars: \" ` and anything in `xah-brackets'.
  This command ignores nesting. For example, if text is
  		(a(b)c▮)
  the selected char is “c”, not “a(b)c”.

  URL `http://xahlee.info/emacs/emacs/modernization_mark-word.html'
  Version: 2020-11-24 2023-07-16 2023-07-23"
  (interactive)
  (let ((xskipChars (concat "^\"`" (mapconcat #'identity xah-brackets ""))))
    (skip-chars-backward xskipChars)
    (push-mark (point) t t)
    (skip-chars-forward xskipChars)))

;; Select Current Line
(defun xah-select-line ()
  "Select current line. If region is active, extend selection downward by line.
  If `visual-line-mode' is on, consider line as visual line.

  URL `http://xahlee.info/emacs/emacs/modernization_mark-word.html'
  Version: 2017-11-01 2021-03-19 2023-07-16"
  (interactive)
  (if (region-active-p)
      (if visual-line-mode
  	      (let ((xp1 (point)))
  	        (end-of-visual-line 1)
  	        (when (eq xp1 (point))
  	          (end-of-visual-line 2)))
  	    (progn
  	      (forward-line 1)
  	      (end-of-line)))
    (if visual-line-mode
  	    (progn (beginning-of-visual-line)
  	           (push-mark (point) t t)
  	           (end-of-visual-line))
      (progn
  	    (push-mark (line-beginning-position) t t)
  	    (end-of-line)))))

;; Select Current Block
(defun xah-select-block ()
  "Select the current/next block plus 1 blankline.
  If region is active, extend selection downward by block.

  URL `http://xahlee.info/emacs/emacs/modernization_mark-word.html'
  Version: 2019-12-26 2021-04-04 2021-08-13"
  (interactive)
  (if (region-active-p)
      (re-search-forward "\n[ \t]*\n[ \t]*\n*" nil 1)
    (progn
      (skip-chars-forward " \n\t")
      (when (re-search-backward "\n[ \t]*\n" nil 1)
  	    (goto-char (match-end 0)))
      (push-mark (point) t t)
      (re-search-forward "\n[ \t]*\n" nil 1))))

;; Extend Selection
(defun xah-extend-selection ()
  "Select the current word, bracket/quote expression, or expand selection.
  Subsequent calls expands the selection.

  when there is no selection,
  • If cursor is on any type of bracket (including parenthesis, quotation mark), select whole bracketed thing including bracket
  • else, select current word.

  when there is a selection, the selection extension behavior is still experimental. But when cursor is on a any type of bracket (parenthesis, quote), it extends selection to outer bracket.

  URL `http://xahlee.info/emacs/emacs/modernization_mark-word.html'
  Version: 2020-02-04 2023-07-22 2023-07-23"
       (interactive)
       (if (region-active-p)
           (progn
             (let ((xrb (region-beginning)) (xre (region-end)))
               (goto-char xrb)
               (cond
                ((looking-at "\\s(")
                 (if (eq (nth 0 (syntax-ppss)) 0)
                     (progn
                       ;; (message "left bracket, depth 0.")
                       (end-of-line) ; select current line
                       (push-mark (line-beginning-position) t t))
                   (progn
                     ;; (message "left bracket, depth not 0")
                     (up-list -1 t t)
                     (mark-sexp))))
                ((eq xrb (line-beginning-position))
                 (progn
                   (goto-char xrb)
                   (let ((xfirstLineEndPos (line-end-position)))
                     (cond
                      ((eq xre xfirstLineEndPos)
                       (progn
                         ;; (message "exactly 1 line. extend to next whole line." )
                         (forward-line 1)
                         (end-of-line)))
                      ((< xre xfirstLineEndPos)
                       (progn
                         ;; (message "less than 1 line. complete the line." )
                         (end-of-line)))
                      ((> xre xfirstLineEndPos)
                       (progn
                         ;; (message "beginning of line, but end is greater than 1st end of line" )
                         (goto-char xre)
                         (if (eq (point) (line-end-position))
                             (progn
                               ;; (message "exactly multiple lines" )
                               (forward-line 1)
                               (end-of-line))
                           (progn
                             ;; (message "multiple lines but end is not eol. make it so" )
                             (goto-char xre)
                             (end-of-line)))))
                      (t (error "%s: logic error 42946" real-this-command))))))
                ((and (> (point) (line-beginning-position)) (<= (point) (line-end-position)))
                 (progn
                   ;; (message "less than 1 line" )
                   (end-of-line) ; select current line
                   (push-mark (line-beginning-position) t t)))
                (t
                 ;; (message "last resort" )
                 nil))))
         (progn
           (cond
            ((looking-at "\\s(")
             ;; (message "left bracket")
             (mark-sexp)) ; left bracket
            ((looking-at "\\s)")
             ;; (message "right bracket")
             (backward-up-list) (mark-sexp))
            ((looking-at "\\s\"")
             ;; (message "string quote")
             (mark-sexp)) ; string quote
            ;; ((and (eq (point) (line-beginning-position)) (not (looking-at "\n")))
            ;;  (message "beginning of line and not empty")
            ;;  (end-of-line)
            ;;  (push-mark (line-beginning-position) t t))
            (
             ;; (prog2 (backward-char) (looking-at "[-_a-zA-Z0-9]") (forward-char))
             (looking-back "[-_a-zA-Z0-9]" (max (- (point) 1) (point-min)))
             ;; (message "left is word or symbol")
             (skip-chars-backward "-_a-zA-Z0-9")
             ;; (re-search-backward "^\\(\\sw\\|\\s_\\)" nil t)
             (push-mark)
             (skip-chars-forward "-_a-zA-Z0-9")
             (setq mark-active t)
             ;; (exchange-point-and-mark)
             )
            ((and (looking-at "[:blank:]")
                  (prog2 (backward-char) (looking-at "[:blank:]") (forward-char)))
             ;; (message "left and right both space" )
             (skip-chars-backward "[:blank:]") (push-mark (point) t t)
             (skip-chars-forward "[:blank:]"))
            ((and (looking-at "\n")
                  (eq (char-before) 10))
             ;; (message "left and right both newline")
             (skip-chars-forward "\n")
             (push-mark (point)  t t)
             (re-search-forward "\n[ \t]*\n")) ; between blank lines, select next block
            (t
             ;; (message "just mark sexp" )
             (mark-sexp)
             (exchange-point-and-mark))
            ;;
            ))))

(defun xah-goto-matching-bracket ()
  "Move cursor to the matching bracket.
  If cursor is not on a bracket, call `backward-up-list'.
  The list of brackets to jump to is defined by `xah-left-brackets' and `xah-right-brackets'.

  URL `http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html'
  Version: 2016-11-22 2023-07-22"
  (interactive)
  (if (nth 3 (syntax-ppss))
  		(backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
  	(cond
  	 ((eq (char-after) ?\") (forward-sexp))
  	 ((eq (char-before) ?\") (backward-sexp))
  	 ((looking-at (regexp-opt xah-left-brackets))
  		(forward-sexp))
  	 ((prog2 (backward-char) (looking-at (regexp-opt xah-right-brackets)) (forward-char))
  		(backward-sexp))

	   (t (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)))))

(general-def
  "M-6" #'xah-select-block
  "M-7" #'xah-select-line
  "M-8" #'xah-extend-selection
  "M-9" #'xah-select-text-in-quote
  "C-<tab>" #'xah-goto-matching-bracket)

(provide 'navigation)
;;; navigation.el ends here
