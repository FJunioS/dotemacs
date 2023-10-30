;; -*- lexical-binding: t; -*-
;;; my-emacs-font.el -- fonts settings 

(defvar my-default-font "FantasqueSansM Nerd Font Mono")
(defvar my-variable-pitch-font "FantasqueSansM Nerd Font Mono")
(defvar my-default-font-size 150)
(defvar my-variable-pitch-font-size 170)

;; markdown
(custom-set-faces
 '(markdown-header-face ((t (:inherit font-lock-function-name-face :weight bold :family Iosevka))))
 '(markdown-header-face-1 ((t (:inherit markdown-header-face :height 1.5))))
 '(markdown-header-face-2 ((t (:inherit markdown-header-face :height 1.4))))
 '(markdown-header-face-3 ((t (:inherit markdown-header-face :height 1.3))))
 '(markdown-header-face-4 ((t (:inherit markdown-header-face :height 1.2))))
 '(markdown-header-face-5 ((t (:inherit markdown-header-face :height 1.0))))
 '(markdown-header-face-6 ((t (:inherit markdown-header-face :height 1.0)))))

;; 'org'
(let* ((variable-tuple
        (cond
         ((x-list-fonts "Iosevka")  '(:family "Iosevka"))
         ((x-list-fonts "PT Serif") '(:family "PT Serif"))))
       (fixed-tuple
        (cond
         ((x-list-fonts "Iosevka Custom") '(:family "Iosevka Custom" :height 160))
         ((x-list-fonts "PT Mono") '(:family "PT Mono" :height 120))))
       (headline `(:inherit default :weight bold)))

  (custom-theme-set-faces
   'user
   `(org-level-1 ((t (,@headline ,@variable-tuple))))
   `(org-level-2 ((t (,@headline ,@variable-tuple))))
   `(org-level-3 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-document-title ((t (,@headline ,@variable-tuple))))
   '(org-ellipsis ((t (:inherit fixed-pitch :height 0.8 :weight normal :foreground "gray40" :underline nil))))
   '(org-block            ((t (:inherit fixed-pitch))))
   '(org-block-begin-line ((t (:inherit fixed-pitch))))
   '(org-block-end-line   ((t (:inherit fixed-pitch))))
   '(org-src              ((t (:inherit fixed-pitch))))
   '(org-properties       ((t (:inherit fixed-pitch))))
   '(org-code             ((t (:inherit (shadow fixed-pitch)))))
   '(org-date             ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info    ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-drawer           ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent           ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link             ((t (:inherit fixed-pitch :underline t))))
   '(org-meta-line        ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value   ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword  ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table            ((t (:inherit fixed-pitch))))
   '(org-tag              ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim         ((t (:inherit (shadow fixed-pitch)))))))

(defun prog-mode-prettify () (interactive)
    (setq prettify-symbols-alist
	    '(
			 ("lambda" . "λ" )
			 ("||" . "∥" )
			 ("|_|" . "⫲" )
			 ("::" . "∷" )

			 ("fn" . "⨍" )

			 ("defun" . "⨍" )
			 ("function" . "⨍" )

			 ;; Elisp specific
			 ("nil" . "∅")
			 ("kbd" . "⌨")
			 ("use-package" . "📦")

			 ("(c)" . "🄫")
			 ("(cc)" . "🄫")

             ("=>" . "⇒")
             ("->" . "→")
			 
			 ("<=" . "⩽")
			 (">=" . "⩾") 
			 ("!=" . "≠")

			 ("j" . "𝐣")
			 ("i" . "𝔦")
			 ("T" . "Ṫ")
			 ("U" . "Ů")
			 ("S" . "Ṡ")
			 ("R" . "Ṙ")

			 ("<." . "⋖")
			 (">." . "⋗")
			 ("'" . "′")))
    (prettify-symbols-mode nil)
    (prettify-symbols-mode 1))
;; ---       
(defun org-mode-prettify () (interactive)
	(setq prettify-symbols-unprettify-at-point t)
    (setq prettify-symbols-alist
		(mapcan (lambda (x) (list x (cons (upcase (car x)) (cdr x))))
			'(("lambda" . "λ")
				 ("||" . "∥")
				 ("|_|" . "⫲")
				 ("->" . "→")
				 ("<-" . "←")
				 ("<->" . "↔")
				 ("::" . "∷" )
				 ("..." . "…")
				 ("(c)" . "🄫")
				 ("(cc)" . "🅭")
				 ("todo" . "" )
				 ("wait" . "" )        
   				 ("nope" . "" )
				 ("done" . "" )
				 ("[#a]" . "" )
				 ("[#b]" . "⬆")
 				 ("[#c]" . "■")
 				 ("[#d]" . "⬇")
 				 ("[#e]" . "❓")
				 ("[ ]" . "" )
				 ("[X]" . "" )
				 ("[-]" . "" )
				 ("#+roam_tags:" . "" )
				 ("#+filetags:" . "" )
				 ("#+name:" . "" )
				 ("#+results:" . "" )
				 ("#+header:" . "" )
				 ("#+author:" . "" )
				 ("#+html_head:" . "" )
				 ("#+subtitle:" . "" )
				 ("#+title" . ".")
				 ("#+begin_src" . "" )
				 ("#+end_src" . "∎" )
				 (":effort:" . "" )
				 ("scheduled:" . "" )
				 ("deadline:" . "" )
				 (":properties:" . "⚙")
				 (":end:" . "―" )
				 ("---" . "―" ))))
	(prettify-symbols-mode nil)
	(prettify-symbols-mode 1))

(setq prettify-symbols-unprettify-at-point 'right-edge)
(add-hook 'org-mode-hook #'org-mode-prettify)
(add-hook 'prog-mode-hook #'prog-mode-prettify)

(lambda ()
	(set-face-attribute 'default nil
		:font my-default-font
		:height my-default-font-size)

	(set-face-attribute 'fixed-pitch nil
		:font my-default-font
		:height my-default-font-size)

	(set-face-attribute 'variable-pitch nil
		:font my-default-font
		:height my-variable-pitch-font-size
		:weight 'regular))

(add-to-list 'default-frame-alist '(font . "Iosevka Custom"))

(set-face-attribute 'font-lock-comment-face nil
		    :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
		    :slant 'normal
		    :weight 'bold)

(setq org-fontify-whole-heading-line t
      org-fontify-done-headline t
      org-fontify-quote-and-verse-blocks t)

(use-package ligature
  :straight (:host github :repo "mickeynp/ligature.el")
  :defer-incrementally t
  :config
  (ligature-set-ligatures t '("www" "..."))

  ;; enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))

  ;; only use ligature for 2 semicolons (unfortunately there is not one for 3/4
  ;; in any font I've seen)
  (ligature-set-ligatures
   noct-lisp-modes
   '((";" (rx (* ";")))))

  (global-ligature-mode t))

(provide 'my-emacs-font)

;;; my-emacs-font.el ends here
