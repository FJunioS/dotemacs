;;; navigation.el ---  desc  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'core-packages)
(require 'core-lib)
(require '+general)
(require '+evil)

(global-set-key [remap keyboard-quit] #'escape)

(general-def 'evil-command-line-map
  "C-k" #'previous-history-element
  "C-j" #'next-history-element)

(general-def 'normal 'woman-node-map
  "]]" 'WoMan-next-manpage
  "[[" 'WoMan-previous-manpage

  "r" 'woman-reformat-last-file) ; refresh

(general-def 'normal emacs-lisp-mode-map
  "K" #'helpful-at-point)

(general-def 'normal prog-mode-map
  "K" #'eldoc-box-help-at-point)

(nvmap
  "<escape>" #'escape
  "5"        #'evil-jump-item
  "6"        #'evil-buffer
  "q"        #'close-minibuffer)

(global-key
  "C-h =" #'describe-char
  "M-k" #'kill-this-buffer
  "M-o" #'evil-switch-to-windows-last-buffer)

(leader
  "-" #'dired-jump)

(leader/open
  "w" #'woman)

(leader/system
  "l" #'evil-window-right
  "k" #'evil-window-up
  "j" #'evil-window-down
  "h" #'evil-window-left

  "i" #'ibuffer
  "d" #'kill-buffer-delete-window)

(leader/file
  "f" #'find-file
  "r" #'consult-recent-file
  "d" #'manual-save-buffer)

(leader/toggle
  "w" #'toggle-word-wrap)

;; ** Packages keymaps
(use-package centaur-tabs
  :init
  (general-after-gui
    (centaur-tabs-mode))
  :general
  (global-key
    "M-[" #'centaur-tabs-backward-group
    "M-]" #'centaur-tabs-forward-group
    "M-p" #'centaur-tabs-forward-tab
    "M-n" #'centaur-tabs-backward-tab)

  (leader/system
    "p" #'centaur-tabs-forward
    "n" #'centaur-tabs-backward)
  (leader
    "[" #'centaur-tabs-backward-group
    "]" #'centaur-tabs-forward-group)
  :config
  (gsetq centaur-tabs-set-icons t
         centaur-tabs-set-bar 'left
         centaur-tabs-down-tab-text "▾"
         centaur-tabs-forward-tab-text "⏵"
         centaur-tabs-backward-tab-text "⏴"
         centaur-tabs-style "rounded"
         centaur-tabs-height 32
         centaur-tabs-show-new-tab-button t
         centaur-tabs-set-modified-marker t
         centaur-tabs-show-navigation-buttons t
         centaur-tabs-left-edge-margin nil)

  (centaur-tabs-change-fonts (face-attribute 'default :font) 110)
  (centaur-tabs-headline-match)
  (setq uniquify-separator "/")
  (setq uniquify-buffer-name-style 'forward)
  :hook
  (dashboard-mode . centaur-tabs-local-mode)
  (term-mode . centaur-tabs-local-mode)
  (calendar-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode))

(use-package drag-stuff
  :init
  (drag-stuff-global-mode)
  :config
  (drag-stuff-define-keys)
  :general
  ('visual
   "K" #'drag-stuff-up
   "J" #'drag-stuff-down))

(use-package evil-mc
  :general
  (general-def 'visual 'evil-mc-key-map
           "A" #'evil-mc-make-cursor-in-visual-selection-end
           "I" #'evil-mc-make-cursor-in-visual-selection-beg)
  :config
  (global-evil-mc-mode))

(use-package multiple-cursors
  :general
  ('normal
   "C-n" #'mc/mark-next-like-this)
  ('visual
   "C-n" #'mc/mark-next-like-this))

(use-package mwim
  :defer t
  :general
  (nvmap
    "0" #'mwim-beginning-of-code-or-line
    "-" #'mwim-end-of-code-or-line)
  (global-key
    "C-a" #'mwim-beginning-of-code-or-line
    "C-e" #'mwim-end-of-code-or-line))

(use-package emms
  :config
  (emms-standard) ;; or (emms-devel) if you want all features
  (setq emms-source-file-default-directory "~/Music"
        emms-info-asynchronously t
        emms-show-format "♪ %s")
  (if (executable-find "mplayer")
      (setq emms-player-list '(emms-player-mplayer))
    (emms-default-players))
  (global-key "M-7" #'emms-smart-browse))

(use-package hl-todo
  :config
  (global-hl-todo-mode))

(defun format-paragraph()
  (interactive)
  ;; TODO: replace this with proper evil command to format paragraph
  (general-simulate-key "=ap")
  (save-excursion
    (general-simulate-=ap)))
(nmap "=" (general-key-dispatch 'evil-indent
            "=" #'format-paragraph))

;; ** Yank/Kill hotfix
(defvar delete-and-paste-args "")

(defun yank-and-register()
  (interactive)
  (call-interactively #'evil-yank)
  (setq delete-and-paste-args (current-kill 0 t)))

(defun kill-and-paste()
  (interactive)
  (kill-region (region-beginning) (region-end))
  (insert (current-kill 0 t)))

(defun delete-and-paste()
  (interactive)
  (let ((Begin (region-beginning))
        (End (region-end)))
      (kill-region Begin End)
      (insert delete-and-paste-args)))

(general-def 'visual
  "y" #'yank-and-register
  "p" #'kill-and-paste
  "P" #'delete-and-paste)

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
  "/" (wrap-region-with-symbol! "/")
  "'" (wrap-region-with-symbol! "'")
  "`" (wrap-region-with-symbol! "`" "'")
  "(" (wrap-region-with-symbol! "(" ")")
  "[" (wrap-region-with-symbol! "[" "]")
  "{" (wrap-region-with-symbol! "{" "}"))

;;; Xah Lee Keymaps:
(defvar xah-brackets '("“”" "()" "[]" "{}" "<>"
		                   "＜＞" "（）" "［］" "｛｝"
		                   "⦅⦆" "〚〛" "⦃⦄" "‹›" "«»"
		                   "「」" "〈〉" "《》" "【】"
		                   "〔〕" "⦗⦘" "『』" "〖〗"
		                   "〘〙" "｢｣" "⟦⟧" "⟨⟩" "⟪⟫"
		                   "⟮⟯" "⟬⟭" "⌈⌉" "⌊⌋" "⦇⦈" "⦉⦊"
		                   "❛❜" "❝❞" "❨❩" "❪❫" "❴❵"
		                   "❬❭" "❮❯" "❰❱" "❲❳" "〈〉"
		                   "⦑⦒" "⧼⧽" "﹙﹚" "﹛﹜" "﹝﹞"
		                   "⁽⁾" "₍₎" "⦋⦌" "⦍⦎" "⦏⦐" "⁅⁆"
		                   "⸢⸣" "⸤⸥" "⟅⟆" "⦓⦔" "⦕⦖"
		                   "⸦⸧" "⸨⸩" "｟｠")
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

(use-package calc-mode
  :elpaca nil
  :config
  (general-def 'normal 'calc-mode-map
    "0" 'calcDigit-start
    "1" 'calcDigit-start
    "2" 'calcDigit-start
    "3" 'calcDigit-start
    "4" 'calcDigit-start
    "5" 'calcDigit-start
    "6" 'calcDigit-start
    "7" 'calcDigit-start
    "8" 'calcDigit-start
    "9" 'calcDigit-start

    "<tab>" 'calc-roll-down
    "S-<return>" 'calc-over
    "RET" 'calc-enter
    "SPC" 'calc-enter

    "C-x C-t" 'calc-transpose-lines
    "C-M-d" 'calc-pop-above
    "C-M-i" 'calc-roll-up
    "M-RET" 'calc-last-args
    "C-M-w" 'kill-ring-save
    "M-%" 'calc-percent
    "M-k" 'calc-copy-as-kill
    "M-w" 'calc-copy-region-as-kill
    "M-DEL" 'calc-pop-above
    "M-m t" 'calc-total-algebraic-mode
    "<delete>" 'calc-pop
    "<mouse-2>" 'calc-yank
    "DEL" 'calc-pop ; was "C-d"
    "d" 'calc-kill                      ; was "C-k"
    "u" 'calc-undo                      ; was "U"
    "X" 'calc-call-last-kbd-macro       ; "@" is already used.
    "pp" 'calc-yank                     ; was "C-y"
    "pP" 'calc-copy-to-buffer           ; was "y"

    "C-p" 'calc-precision         ; was "p"

    "?" 'calc-help
    ;; "h" 'calc-help-prefix ; TODO: Rebind?
    "i" 'calc-info

    "\"" 'calc-auto-algebraic-entry
    "$" 'calc-auto-algebraic-entry      ; TODO: No need for this one?
    "'" 'calc-algebraic-entry

    "!" 'calc-factorial
    "#" 'calcDigit-start
    "%" 'calc-mod
    "&" 'calc-inv
    "(" 'calc-begin-complex
    ")" 'calc-end-complex
    "*" 'calc-times
    "+" 'calc-plus
    "," 'calc-comma
    "-" 'calc-minus
    "." 'calcDigit-start
    "/" 'calc-divide
    ":" 'calc-fdiv
    ";" 'calc-semi         ; TODO: Shall we really override `evil-ex'?
    "<" 'calc-scroll-left
    "=" 'calc-evaluate
    ">" 'calc-scroll-right
    "@" 'calcDigit-start
    "A" 'calc-abs
    "B" 'calc-log
    "C" 'calc-cos
    "C-r" 'calc-redo
    "E" 'calc-exp
    "F" 'calc-floor
    "G" 'calc-argument
    "H" 'calc-hyperbolic
    "I" 'calc-inverse
    "J" 'calc-conj
    "K" 'calc-keep-args
    "L" 'calc-ln
    "M" 'calc-more-recursion-depth
    "N" 'calc-eval-num
    "O" 'calc-option
    "P" 'calc-pi
    "Q" 'calc-sqrt
    "R" 'calc-round
    "S" 'calc-sin
    "T" 'calc-tan
    "[[" 'calc-begin-vector
    "]]" 'calc-end-vector
    "\\" 'calc-idiv
    "^" 'calc-power
    "_" 'calcDigit-start
    "`" 'calc-edit
    "e" 'calcDigit-start
    "n" 'calc-change-sign
    "o" 'calc-realign
    "w" 'calc-why
    "x" 'calc-execute-extended-command
    "|" 'calc-concat
    "{" 'calc-scroll-down               ; TODO: Not necessary?
    "}" 'calc-scroll-up                 ; TODO: Not necessary?
    "~" 'calc-num-prefix

    ;; quit
    "q" 'calc-quit

    "V" (lookup-key calc-mode-map (kbd "V"))
    "Y" (lookup-key calc-mode-map (kbd "Y"))
    "Z" (lookup-key calc-mode-map (kbd "Z"))
    "a" (lookup-key calc-mode-map (kbd "a"))
    "b" (lookup-key calc-mode-map (kbd "b"))
    "c" (lookup-key calc-mode-map (kbd "c"))
    "D" (lookup-key calc-mode-map (kbd "d"))
    "f" (lookup-key calc-mode-map (kbd "f"))
    "g" (lookup-key calc-mode-map (kbd "g"))
    "zj" (lookup-key calc-mode-map (kbd "j"))
    "zk" (lookup-key calc-mode-map (kbd "k"))
    "zl" (lookup-key calc-mode-map (kbd "l"))
    "m" (lookup-key calc-mode-map (kbd "m"))
    "r" (lookup-key calc-mode-map (kbd "r"))
    "s" (lookup-key calc-mode-map (kbd "s"))
    "t" (lookup-key calc-mode-map (kbd "t"))
    "U" (lookup-key calc-mode-map (kbd "u"))
    "v" (lookup-key calc-mode-map (kbd "v"))
    "zz" (lookup-key calc-mode-map (kbd "z"))))

(use-package info
  :elpaca nil
  :config
  (general-def 'normal 'Info-mode-map
    "<tab>" 'Info-next-reference
    "S-<tab>" 'Info-prev-reference
    "g TAB" 'Info-next-reference
    "g]" 'Info-next-reference
    "g[" 'Info-prev-reference

    "h" 'Info-help
    "l" 'Info-history-back
    "C-o" 'Info-history-back
    " " 'Info-scroll-up
    "RET" 'Info-follow-nearest-node
    "C-]" 'Info-follow-nearest-node
    "DEL" 'Info-scroll-down
    "C-i" 'Info-history-forward

    "d" 'Info-directory
    "u" 'Info-up
    "gL" 'Info-history ; "L"
    "s" 'Info-search
    "S" 'Info-search-case-sensitively
    "i" 'Info-index
    "I" 'Info-virtual-index
    "a" 'info-apropos

    ;; mouse integration
    [mouse-2]     'Info-mouse-follow-nearest-node
    [follow-link] 'mouse-face
    ;; make mac user happy
    [double-wheel-left]  'Info-history-back
    [double-wheel-right] 'Info-history-forward

    ;; digit arguments
    "g1" 'Info-nth-menu-item
    "g2" 'Info-nth-menu-item
    "g3" 'Info-nth-menu-item
    "g4" 'Info-nth-menu-item
    "g5" 'Info-nth-menu-item
    "g6" 'Info-nth-menu-item
    "g7" 'Info-nth-menu-item
    "g8" 'Info-nth-menu-item
    "g9" 'Info-nth-menu-item

    ;; goto
    "J" 'Info-menu

    "gG" 'Info-goto-node
    "gm" 'Info-menu
    "gt" 'Info-top-node
    "gT" 'Info-toc
    "gf" 'Info-follow-reference
    "C-j" 'Info-forward-node
    "C-k" 'Info-backward-node
    "gj" 'Info-next
    "gk" 'Info-prev

    "g," 'Info-index-next

    "g?" 'Info-summary))


(use-package imenu
  :elpaca nil
  :config
  (general-with 'evil
    (evil-add-command-properties 'imenu :jump t)
    (evil-declare-not-repeat 'imenu))
  (general-def 'normal 'imenu-list-major-mode-map
    "RET" 'imenu-list-goto-entry
    "TAB" 'hs-toggle-hiding
    "d" 'imenu-list-display-entry
    "gr" 'imenu-list-refresh
    "q" 'imenu-list-quit-window))

(use-package ibuffer
  :elpaca nil
  :config
  (general-def 'normal 'ibuffer-mode-map
    "=" 'ibuffer-diff-with-file
    "J" 'ibuffer-jump-to-buffer
    "M-g" 'ibuffer-jump-to-buffer
    "M-s a C-s" 'ibuffer-do-isearch
    "M-s a M-C-s" 'ibuffer-do-isearch-regexp
    "M-s a C-o" 'ibuffer-do-occur

    ;; mark
    "m" 'ibuffer-mark-forward
    "~" 'ibuffer-toggle-marks
    "u" 'ibuffer-unmark-forward
    "DEL" 'ibuffer-unmark-backward
    "M-DEL" 'ibuffer-unmark-all
    "* *" 'ibuffer-mark-special-buffers
    "* c" 'ibuffer-change-marks
    "U" 'ibuffer-unmark-all-marks
    "* M" 'ibuffer-mark-by-mode
    "* m" 'ibuffer-mark-modified-buffers
    "* u" 'ibuffer-mark-unsaved-buffers
    "* s" 'ibuffer-mark-special-buffers
    "* r" 'ibuffer-mark-read-only-buffers
    "* /" 'ibuffer-mark-dired-buffers
    "* e" 'ibuffer-mark-dissociated-buffers
    "* h" 'ibuffer-mark-help-buffers
    "* z" 'ibuffer-mark-compressed-file-buffers
    "." 'ibuffer-mark-old-buffers

    "d" 'ibuffer-mark-for-delete
    "x" 'ibuffer-do-kill-on-deletion-marks

    ;; immediate operations
    "gj" 'ibuffer-forward-line
    "gk" 'ibuffer-backward-line

    "}" 'ibuffer-forward-next-marked
    "{" 'ibuffer-backwards-next-marked
    "M-}" 'ibuffer-forward-next-marked
    "M-{" 'ibuffer-backwards-next-marked

    "gR" 'ibuffer-redisplay
    "gr" 'ibuffer-update

    "`" 'ibuffer-switch-format
    "-" 'ibuffer-add-to-tmp-hide
    "+" 'ibuffer-add-to-tmp-show
    "X" 'ibuffer-bury-buffer
    "," 'ibuffer-toggle-sorting-mode
    "o i" 'ibuffer-invert-sorting
    "o a" 'ibuffer-do-sort-by-alphabetic
    "o v" 'ibuffer-do-sort-by-recency
    "o s" 'ibuffer-do-sort-by-size
    "o f" 'ibuffer-do-sort-by-filename/process
    "o m" 'ibuffer-do-sort-by-major-mode

    "s RET" 'ibuffer-filter-by-mode
    "s m" 'ibuffer-filter-by-used-mode
    "s M" 'ibuffer-filter-by-derived-mode
    "s n" 'ibuffer-filter-by-name
    "s *" 'ibuffer-filter-by-starred-name
    "s f" 'ibuffer-filter-by-filename
    "s b" 'ibuffer-filter-by-basename
    "s ." 'ibuffer-filter-by-file-extension
    "s <" 'ibuffer-filter-by-size-lt
    "s >" 'ibuffer-filter-by-size-gt
    "s i" 'ibuffer-filter-by-modified
    "s v" 'ibuffer-filter-by-visiting-file
    "s c" 'ibuffer-filter-by-content
    "s e" 'ibuffer-filter-by-predicate

    "s r" 'ibuffer-switch-to-saved-filters
    "s a" 'ibuffer-add-saved-filters
    "s x" 'ibuffer-delete-saved-filters
    "s d" 'ibuffer-decompose-filter
    "s s" 'ibuffer-save-filters
    "s p" 'ibuffer-pop-filter
    "s <up>" 'ibuffer-pop-filter
    "s !" 'ibuffer-negate-filter
    "s t" 'ibuffer-exchange-filters
    "s TAB" 'ibuffer-exchange-filters
    "s o" 'ibuffer-or-filter
    "s |" 'ibuffer-or-filter
    "s &" 'ibuffer-and-filter
    "s g" 'ibuffer-filters-to-filter-group
    "s P" 'ibuffer-pop-filter-group
    "s S-<up>" 'ibuffer-pop-filter-group
    "s D" 'ibuffer-decompose-filter-group
    "s /" 'ibuffer-filter-disable

    "C-j" 'ibuffer-forward-filter-group
    "M-n" 'ibuffer-forward-filter-group
    "]]" 'ibuffer-forward-filter-group
    "\t" 'ibuffer-forward-filter-group
    "M-p" 'ibuffer-backward-filter-group
    "C-k" 'ibuffer-backward-filter-group
    "[[" 'ibuffer-backward-filter-group
    [backtab] 'ibuffer-backward-filter-group
    "M-j" 'ibuffer-jump-to-filter-group
    "gx" 'ibuffer-kill-line
    "C-y" 'ibuffer-yank
    "s S" 'ibuffer-save-filter-groups
    "s R" 'ibuffer-switch-to-saved-filter-groups
    "s X" 'ibuffer-delete-saved-filter-groups
    "s \\" 'ibuffer-clear-filter-groups

    "% n" 'ibuffer-mark-by-name-regexp
    "% m" 'ibuffer-mark-by-mode-regexp
    "% f" 'ibuffer-mark-by-file-name-regexp
    "% g" 'ibuffer-mark-by-content-regexp
    "% L" 'ibuffer-mark-by-locked

    "C-t" 'ibuffer-visit-tags-table

    "|" 'ibuffer-do-shell-command-pipe
    "!" 'ibuffer-do-shell-command-file
    "t" 'ibuffer-toggle-marks
    ;; marked operations
    "A" 'ibuffer-do-view
    "D" 'ibuffer-do-delete
    "E" 'ibuffer-do-eval
    "F" 'ibuffer-do-shell-command-file
    "I" 'ibuffer-do-query-replace-regexp
    "H" 'ibuffer-do-view-other-frame
    "N" 'ibuffer-do-shell-command-pipe-replace
    "M" 'ibuffer-do-toggle-modified
    "O" 'ibuffer-do-occur
    "P" 'ibuffer-do-print
    "Q" 'ibuffer-do-query-replace
    "R" 'ibuffer-do-rename-uniquely
    "S" 'ibuffer-do-save
    "T" 'ibuffer-do-toggle-read-only
    "r" 'ibuffer-do-replace-regexp
    "V" 'ibuffer-do-revert
    "W" 'ibuffer-do-view-and-eval

    "K" 'ibuffer-do-kill-lines
    "yf" 'ibuffer-copy-filename-as-kill
    "yb" 'ibuffer-copy-buffername-as-kill

    "RET" 'ibuffer-visit-buffer
    "go" 'ibuffer-visit-buffer-other-window
    "C-o" 'ibuffer-visit-buffer-other-window-noselect
    "M-o" 'ibuffer-visit-buffer-1-window
    "gv" 'ibuffer-do-view
    "gV" 'ibuffer-do-view-horizontally

    ;; Quit
    "q" 'quit-window))

(provide 'navigation)
;;; navigation.el ends here
