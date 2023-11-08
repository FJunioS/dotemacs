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
         whitespace-line-column 120
         whitespace-display-mappings
         '((tab-mark ?\t [?\xBB ?\t])
           (newline-mark ?\n [?¬ ?\n])
           (trailing-mark ?\n [?¬ ?\n])
           (space-mark ?\xB7 [?·] [?.])
           (space-mark ?\xA0 [?\·] [?_])))

  (defun add-lines-tail ()
    "Add lines-tail to `whitespace-style' and refresh `whitespace-mode'."
    (setq-local whitespace-style (cons 'lines-tail whitespace-style))
    (whitespace-mode))

  (general-add-hook 'prog-mode-hook #'add-lines-tail)

  (global-whitespace-mode)

  (defun manual-save-buffer ()
    (interactive)
    (call-interactively #'delete-trailing-whitespace)
    (call-interactively #'save-buffer))

  (general-def 'normal
       "fd" #'manual-save-buffer
       "C-x C-s" #'manual-save-buffer))

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

(use-package artbollocks-mode
  :ghook '(org-mode-hook text-mode-hook))

(use-package jinx
  :init
  (noct-after-buffer (global-jinx-mode))
  :config
  (global-key "C-." #'jinx-correct))

(provide 'text-editing)
