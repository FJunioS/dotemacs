;;; my-emacs-completion.el --- Packages and functions for better workflow
(require 'my-completion)

(use-package nerd-icons)
(use-package all-the-icons)
(use-package all-the-icons-completion
  :config
  (all-the-icons-completion-mode)
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup))

;;; Hydra :: List available keys when needed
(use-package hydra :commands defhydra)
(use-package use-package-hydra :straight 't)
(straight-use-package 'posframe)
(use-package hydra-posframe :straight (hydra-posframe :host github :repo "Ladicle/hydra-posframe")
  :after hydra)

;;; Orderless :: Enhance search by 
(use-package orderless
  :demand t
  :init
  (setq orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
	orderless-style-dispatchers (list #'+jun/orderless-consult-dispatch
					  #'orderless-affix-dispatch))
  ;; This only works after orderless get loaded
  (with-eval-after-load 'orderless
    (orderless-define-completion-style +orderless-with-initialism
      (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp))))
  :config
  (setq completion-styles '(orderless basic))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides
	'((command (styles +orderless-with-initialism))
	  (symbol (styles +orderless-with-initialism))
	  (variable (styles +orderless-with-initialism)))));;

(use-package pcmpl-args)

(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("M-+ p" . completion-at-point) ;; capf
         ("M-+ t" . complete-tag)        ;; etags
         ("M-+ d" . cape-dabbrev)        ;; or dabbrev-completion
         ("M-+ h" . cape-history)
         ("M-+ f" . cape-file)
         ("M-+ k" . cape-keyword)
         ("M-+ s" . cape-symbol)
         ("M-+ a" . cape-abbrev)
         ("M-+ l" . cape-line)
         ("M-+ w" . cape-dict)
         ("M-+ \\" . cape-tex)
         ("M-+ _" . cape-tex)
         ("M-+ ^" . cape-tex)
         ("M-+ &" . cape-sgml)
         ("M-+ r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; NOTE: The order matters!
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  (add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
 )

;;; Corfu :: Minimalist Completion-at-point engine
(use-package corfu
  :straight
  '(:source gnu-elpa-mirror 
	    :repo "minad/corfu" 
	    :files ("*/corfu*.el" "corfu.el"))
  :hook
  (prog-mode . corfu-mode)
  (shell-mode . corfu-mode)
  (eshell-mode-hook . (lambda ()
			(setq-local corfu-auto nil)
			(corfu-mode)))
  :init
  (setq
   corfu-cycle t        
   corfu-auto t               ;; Enable auto completion
   corfu-separator ?\s         ;; Orderless field separator
   corfu-auto-delay 0.2
   corfu-auto-prefix 1        ;; Minimum amount to enable completion
   corfu-quit-at-boundary nil   ;; Never quit at completion
   corfu-quit-no-match nil      ;; Never quit, even if there is no match
   corfu-preview-current t    ;; Disable current candidate preview
   corfu-preselect 'valid    ;; Preselect the prompt
   corfu-scroll-margin 5      ;; Use scroll margin
   corfu-quit-no-match 'separator
   completion-styles '(orderless))

  :config
  (corfu-popupinfo-mode +1)
  (corfu-history-mode +1)
  (corfu-echo-mode +1)
  (corfu-indexed-mode +1)
  (setq corfu-indexed-start '1)
  iter-empty
  (setq tab-always-indent 'complete)
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  (advice-add #'corfu-insert :after #'+jun/corfu-send-shell));;

(straight-use-package
 '(corfu-terminal
   :type git
   :repo "https://codeberg.org/akib/emacs-corfu-terminal.git"))
(unless (display-graphic-p)
  (corfu-terminal-mode +1))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

;;; Vertico :: Minimalist and customizable command for the next gen. (look alike: Ivy, Helm, IDO and many others)
(use-package vertico
  :config
  (setq vertico-scroll-margin 30)
  (setq vertico-count 10)
  (setq vertico-resize nil)
  (setq vertico-cycle t)
  (setq vertico-preselect 'prompt)
  (setq read-buffer-completion-ignore-case t
	read-file-name-completion-ignore-case t
	completion-ignore-case t)
  :init
  (vertico-mode +1));;

;;; Marginalia :: Give more context to vertico's items
(use-package marginalia
  :ensure t
  :custom
  (marginalia-separator " ┊ ")
  (marginalia-align-offset 30)
  :init (marginalia-mode));;

;;; Embark :: Enable actions while completing
(use-package embark
  :ensure t
  :custom
  (embark-indicators
   '(embark-verbose-indicator
     embark-highlight-indicator
     embark-isearch-highlight-indicator))
  :config
  (load-library "embark-org")
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none))))
  
  (advice-add #'embark-completing-read-prompter
	      :around #'+jun/embark-hide-which-key-indicator));;

;;;
(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(require 'my-consult)
;;; Consult :: Search mechanism (look alike: Consel)
(use-package consult
  :ensure t
  :config
  (defvar my-consult-line-map
    (let ((map (make-sparse-keymap)))
      (define-key map "\C-s" #'previous-history-element)
      map))
  :custom
  ;;Help!
  (consult-find-args "find . -not ( -wholename */.* -name node_modules -prune -o ) ")
  (consult-project-root-function #'projectile-project-root)
  (consult-customize consult-line :keymap my-consult-line-map)
  (consult-narrow-key "<")
  (consult-preview-key 'any));;

(provide 'my-emacs-completion)

;;; my-completion.el ends here
