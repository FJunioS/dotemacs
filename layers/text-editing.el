;; -*- lexical-binding: t; -*-

(require 'popup-handler)

;; Settings --------------------------------
(setq default-input-method "korean-hangul")

(setq mark-even-if-inactive t)
(setq kill-whole-line t)

;; Dabrev
(setq dabbrev-case-fold-search t)
(setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_"
      dabbrev-abbrev-skip-leading-regexp "\\$\\|\\*\\|/\\|=")

(add-hook 'text-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'toggle-word-wrap)

;; Save clipboard before changing it
(setq save-interprogram-paste-before-kill t)

(global-set-key (kbd "C-S-d") 'duplicate-line)
;; Key to kill-whole-line
(global-set-key (kbd "C-S-k") 'kill-whole-line)
(global-set-key (kbd "C-k") 'kill-visual-line-join)


(global-set-key "\C-z" 'zap-up-to-char)
(global-set-key "\C-a" 'back-to-indentation-or-beginning)

;; Use M-j to join lines. C-j splits them, so it's all good.
(global-set-key (kbd "M-j") 'ju-join-line)

;; Easily comment and uncomment region
(global-set-key (kbd "C-c ;") 'comment-region)

;;(define-key global-map (kbd "M-o") 'open-previous-line)
;;(define-key global-map (kbd "C-j") 'open-next-line)

(define-key global-map (kbd "C-'") 'open-previous-line)
(define-key global-map (kbd "M-'") 'use-register-dwim)

;; --------------------------------------
;;              Functions
;; --------------------------------------

(defun join-line-below ()
  "Join line below instead of current, keeping cursor on place."
  (interactive)
  (save-excursion
    (forward-line)
    (join-line)))

(defun kill-visual-line-join ()
  "When point is on eol, join line below, if is bol, kill line and join."
  (interactive)
  (cond
   ((bolp)
    (save-excursion
      (kill-visual-line)
      (delete-char)))
   ((eolp)
    (progn
      (join-line-below)
      (indent-according-to-mode)))
   (t (kill-visual-line))))

(defun get-region-pos ()
  (cl-destructuring-bind (beg . end)
      (if (region-active-p)
          (cons (region-beginning)
                (region-end))
        (cons (line-beginning-position)
              (line-end-position)))
    (cons beg end)))

(defun my-kill-ring-save ()
  "An `kill-ring-save' wrapper.
If no active region, yank from point to eol, instead of mark."
  (interactive)
  (if (equal mark-active nil)
      (kill-ring-save (point) (line-end-position))
    (kill-ring-save (point) (mark))))

(global-set-key "\M-w" 'my-kill-ring-save)
(global-set-key "\C-y" 'yank)

(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")
;;; behave like vi's O command
(defun open-previous-line (&optional arg)
  "Open a new line before the current one.
  See also `newline-and-indent'."
  (interactive "p")
  (save-excursion
    (beginning-of-line)
    (open-line (cond (arg arg) (t 1)))
    (unless arg
      (when newline-and-indent
        (indent-according-to-mode))))

  (when (and (eolp)
             (bolp))
    (next-line)))

(defun open-next-line (&optional arg)
  "Open a new line before the current one.
  See also `newline-and-indent'."
  (interactive "p")
  (save-excursion
    (next-line)
    (beginning-of-line)
    (open-line (cond (arg arg) (t 1)))
    (unless arg
      (indent-according-to-mode))))

;;;###autoload
(defun duplicate-line (&optional arg)
  "Duplicate it. With prefix ARG, duplicate ARG lines following the current one."
  (interactive "p")
  (cl-destructuring-bind (beg . end) (if (region-active-p)
                                         (cons (region-beginning)
                                               (region-end))
                                       (cons (line-beginning-position)
                                             (line-end-position)))
    (embark-insert arg)))

(defun backward-kill-word-or-region (&optional arg)
  "Kill word backward if region is inactive; else kill region"
  (interactive "p")
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))
;; --------------------------------------

;;;###autoload
(defun store-register-dwim (arg register)
  "Store what I mean in a register.
With an active region, store or append (with \\[universal-argument]) the
contents, optionally deleting the region (with a negative
argument). With a numeric prefix, store the number. With \\[universal-argument]
store the frame configuration. Otherwise, store the point."
  (interactive
   (list current-prefix-arg
         (register-read-with-preview "Store in register: ")))
  (cond
   ((use-region-p)
    (let ((begin (region-beginning))
          (end (region-end))
          (delete-flag (or (equal arg '-)  (equal arg '(-4)))))
      (if (consp arg)
          (append-to-register register begin end delete-flag)
        (copy-to-register register begin end delete-flag t))))
   ((numberp arg) (number-to-register arg register))
   (t (point-to-register register arg))))

;;;###autoload
(defun use-register-dwim (register &optional arg)
  "Do what I mean with a register.
For a window configuration, restore it. For a number or text, insert it.
For a location, jump to it."
  (interactive
   (list (register-read-with-preview "Use register: ")
         current-prefix-arg))
  (condition-case nil
      (jump-to-register register arg)
    (user-error (insert-register register arg))))

;;;###autoload
(defun save-buffer-if-visiting-file (&optional args)
  "Save the current buffer only if it is visiting a file"
  (interactive)
  (if (buffer-file-name)
      (save-buffer args)))
(add-hook 'auto-save-hook 'save-buffer-if-visiting-file)
;; ------------------------------------------

(defun back-to-indentation-or-beginning () (interactive)
       (if (= (point) (progn (beginning-of-line-text) (point)))
           (beginning-of-line)))

;; Packages --------------------------------

(use-package visual-fill-column
  :hook ((text-mode-hook . visual-fill-column-mode)
          (prog-mode-hook . visual-fill-column-mode))
  :init
  (setq-default visual-fill-column-width 150)
  (setq-default visual-fill-column-center-text t))

(use-package selection-highlight-mode
  :ensure (:host github
                 :repo "balloneij/selection-highlight-mode")
  :config (selection-highlight-mode))

(use-package indent-guide
  :config
  (add-hook 'prog-mode 'indent-guide-mode)
  (set-face-background 'indent-guide-face "unspecified"))

(use-package whitespace
  :ensure nil
  :demand t
  :config
  (map ju-toggle-map "w" #'whitespace-mode)
  (csetq whitespace-style
         '(face tabs spaces trailing lines space-before-tab
                newline indentation empty space-after-tab space-mark
                tab-mark newline-mark missing-newline-at-eof)
         ;; use `fill-column' value
         whitespace-line-column 120
         whitespace-display-mappings
         '((tab-mark ?\t [?\xBB ?\t])
           (newline-mark ?\n [?¬ ?\n])
           (trailing-mark ?\n [?¬ ?\n])))

  (defun add-lines-tail ()
    "Add lines-tail to `whitespace-style' and refresh `whitespace-mode'."
    (setq-local whitespace-style (cons 'lines-tail whitespace-style))
    (whitespace-mode))

  (add-hook 'prog-mode-hook #'add-lines-tail))

(setq prettify-symbols-unprettify-at-point 'right-edge)

(defvar default-prettify-alist ())
(setq default-prettify-alist
      '(("lambda" . "λ")))

(defun default-prettify-mode()
  "Enable a prettify with custom symbols"
  (interactive)
  (setq prettify-symbols-alist default-prettify-alist)
  (prettify-symbols-mode -1)
  (prettify-symbols-mode +1)
  (setq prettify-symbols-unprettify-at-point 'right-edge))

(add-hook! '(prog-mode-hook text-mode-hook) #'default-prettify-mode)

(defvar org-prettify-alist
  '(("[#a]"  . ? )
    ("[#b]"  . ?⬆)
    ("[#c]"  . ?■)
    ("[#d]"  . ?⬇)
    ("[#e]"  . ?❓)
    ("[ ]"   . ? )
    ("[X]"   . ? )
    ("[-]"   . "" )
    ("#+results:"   . ? )
    ("#+begin_src"  . ? )
    ("#+end_src"    . ?∎ )
    (":end:"        . ?―)))

;; Up-case all keys so "begin_src" and "BEGIN_SRC" has the same icon
(setq org-prettify-alist
      (append (mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
                      org-prettify-alist)
              org-prettify-alist))

(setq org-prettify-alist
      (append default-prettify-alist org-prettify-alist))

(defun org-prettify-mode()
  (interactive)
  (setq prettify-symbols-alist org-prettify-alist)
  (prettify-symbols-mode -1)
  (prettify-symbols-mode +1)
  (setq prettify-symbols-unprettify-at-point 'right-edge))

(add-hook! 'org-mode-hook #'org-prettify-mode)

(use-package dictionary
  :ensure t
  :bind (:map text-mode-map
              ("M-." . dictionary-lookup-definition))
  :init
  (add-to-list 'display-buffer-alist
               '("^\\*dictionary\\*" display-buffer-in-direction
                 (side . right)
                 (window-width . 50)))
  :custom
  (dictionary-server "dict.org"))

(use-package artbollocks-mode)

(use-package jinx
  :disabled
  :init
  (noct-after-buffer (global-jinx-mode))
  :config
  (global-key "C-." #'jinx-correct))

(provide 'text-editing)
;;; text-editing.el ends here.
