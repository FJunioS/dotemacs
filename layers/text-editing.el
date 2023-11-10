(require 'core-packages)
(require 'popup-handler)

(use-package visual-fill-column
  :ghook 'text-mode-hook 'prog-mode-hook
  :init
  (setq-default visual-fill-column-width 150)
  (setq-default visual-fill-column-center-text t))

(use-package hungry-delete
  :general
  ('override
   [remap backward-kill-word]
   (general-predicate-dispatch #'backward-kill-word
     (looking-back (rx (1+ space))
                   (line-beginning-position))
     #'hungry-delete-backward
     (bound-and-true-p lispyville-mode)
     #'lispyville-delete-backward-word)))

(general-add-advice '(backward-kill-word
                      hungry-delete-backward
                      hungry-delete-forward)
                    :around #'NOP-kill-new)

(use-package indent-guide
  :config
  (add-hook 'prog-mode 'indent-guide-mode)
  (set-face-background 'indent-guide-face "unspecified"))

(setq meow-keypad-leader-dispatch "C-c")



(use-package whitespace
  :elpaca nil
  :demand t
  :general (general-def 'leader-toggle-map "w" #'whitespace-mode)

  :config
  (setq whitespace-style
         '(face tabs spaces trailing lines space-before-tab
                newline indentation empty space-after-tab space-mark
                tab-mark newline-mark missing-newline-at-eof)
         ;; use `fill-column' value
         whitespace-line-column 80
         show-trailing-whitespace t
         whitespace-display-mappings
         '((tab-mark      ?\t [?» ?\t])
           (space-mark 32 [?\s] [46])
           (newline-mark  ?\n [?¬ ?\n]))))

  (defun add-lines-tail ()
    "Add lines-tail to `whitespace-style' and refresh `whitespace-mode'."
    (setq-local whitespace-style (cons 'lines-tail whitespace-style))
    (whitespace-mode 1))
  (general-add-hook 'prog-mode-hook #'add-lines-tail)
  (add-hook 'prog-mode 'whitespace-mode)
  (add-hook 'text-mode 'whitespace-mode)
  (add-hook 'org-mode 'whitespace-mode)

  (defun manual-save-buffer ()
    (interactive)
    (call-interactively #'delete-trailing-whitespace)
    (call-interactively #'save-buffer))

  (general-def "C-x C-s" #'manual-save-buffer)

(setq prettify-symbols-unprettify-at-point 'right-edge)

(defvar default-prettify-alist ())
(setq default-prettify-alist
       '(("lambda" . "λ")

         ("<." . "⋖")
         (">." . "⋗")

         ("->"  . "→")
         ("<-"  . "←")
         ("<->" . "↔")
         ("=>"  . "⇒" )
         ("<=>" . "⇔" )

         ("!="  . "≠")
         ("<="  . "⩽")
         (">="  . "⩾")
         ("..." . "…")
         ("++"  . "⧺" )
         ("+++" . "⧻" )
         ("=="  . "⩵" )
         ("===" . "⩶" )

         ("||-" . "⫦" )
         ("|>"  . "⊳" )
         ("<|"  . "⊲" )
         ("<||" . "⧏" )
         ("||>" . "⧐" )

         ("nil" . "∅")
         ("kbd" . "⌨")
         ("use-package" . "📦")

         ("--"    . "―" )
         ("---"   . "―")))

(defun default-prettify-mode()
  "Enable a prettify with custom symbols"
  (interactive)
  (setq prettify-symbols-alist default-prettify-alist)
  (prettify-symbols-mode -1)
  (prettify-symbols-mode +1)
  (setq prettify-symbols-unprettify-at-point 'right-edge))

(use-package meow
  :init
  (meow-global-mode)
  (defun meow-setup ()
    (meow-leader-define-key
   '("?" . meow-cheatsheet))
  (meow-motion-overwrite-define-key
   ;; custom keybinding for motion state
   '("<escape>" . ignore))
  (meow-normal-define-key
   '("?" . meow-cheatsheet)
   '("*" . meow-expand-0)
   '("=" . meow-expand-9)
   '("!" . meow-expand-8)
   '("[" . meow-expand-7)
   '("]" . meow-expand-6)
   '("{" . meow-expand-5)
   '("+" . meow-expand-4)
   '("}" . meow-expand-3)
   '(")" . meow-expand-2)
   '("(" . meow-expand-1)
   '("1" . digit-argument)
   '("2" . digit-argument)
   '("3" . digit-argument)
   '("4" . digit-argument)
   '("5" . digit-argument)
   '("6" . digit-argument)
   '("7" . digit-argument)
   '("8" . digit-argument)
   '("9" . digit-argument)
   '("0" . digit-argument)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-line)
   '("E" . meow-goto-line)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-join)
   '("k" . meow-kill)
   '("l" . meow-till)
   '("m" . meow-mark-word)
   '("M" . meow-mark-symbol)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)
   '("q" . meow-quit)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-search)
   '("t" . meow-right)
   '("T" . meow-right-expand)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-next-word)
   '("W" . meow-next-symbol)
   '("x" . meow-save)
   '("X" . meow-sync-grab)
   '("y" . meow-yank)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))
  (meow-setup)
)
(add-hook! '(prog-mode-hook
             text-mode-hook) #'default-prettify-mode)

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
(use-package image-roll
  :elpaca (:host github :repo "dalanicolai/image-roll.el")
  :config
  (add-hook 'pdf-view-mode 'image-roll-autoloads))

(use-package artbollocks-mode
  :ghook '(org-mode-hook text-mode-hook))

(use-package jinx
  :init
  (noct-after-buffer (global-jinx-mode))
  :config
  (global-set-key (kbd "C-.") #'jinx-correct))

(provide 'text-editing)
