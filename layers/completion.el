(require 'core-packages)

(use-package yasnippet
  :init (yas-global-mode)
  :general
  (leader/yasnippet
    "" nil
    "i" #'(yas-insert-snippet :wk "Insert snippet")
    "n" #'(yas-new-snippet :wk "New snippet")
    "e" #'(yas-visit-snippet-file :wk "Edit snippet"))

  :config
  (add-hook 'escape-hook #'yas-abort-snippet)
  (let ((yas-dir (expand "snippets/" emacs-dir)))
    (eval
     `(setq yas/snippet-dirs '(,yas-dir ,(expand "snippets/" user-emacs-directory )))))

  (setq yas-wrap-around-region t))

(use-package corfu
  :elpaca (corfu :files (:defaults "extensions/*.el"))
  :ghook
  'prog-mode-hook
  'shell-mode-hook
  'eshell-mode-hook
  'eglot-managed-mode-hook

  :config
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  (dolist (c '(minibuffer-setup-hook eshell-mode-hook))
    (add-hook c #'corfu-enable-in-minibuffer))

  (setq corfu-auto t
        corfu-preview-current nil
        corfu-auto-delay 0.15
        corfu-quit-no-match t
        corfu-auto-prefix 3)

  (def 'insert "C-k" nil)

  (imap "C-SPC" #'completion-at-point)
  (general-def 'corfu-map
    "C-SPC" #'corfu-insert-separator
    "C-i" #'corfu-insert
    "C-j" #'corfu-next
    "C-k" #'corfu-previous
    "C-l" #'corfu-complete
    "C-u" #'corfu-scroll-down
    "C-d" #'corfu-scroll-up
    "TAB" #'corfu-complete
    "RET" #'(lambda () (interactive)
              (corfu-complete)
              (general-simulate-key "RET")
              (general-simulate-RET)))

  (dolist (c (list (cons "SPC" " ")
                   (cons "." ".")
                   (cons "C-1" "1")
                   (cons "C-2" "2")
                   (cons "C-3" "3")
                   (cons "C-4" "4")
                   (cons "C-5" "5")
                   (cons "C-6" "6")
                   (cons "C-7" "7")
                   (cons "C-8" "8")
                   (cons "C-9" "9")
                   (cons "C-0" "0")
                   (cons "C-(" "\\(")
                   (cons "C-)" "\\)")
                   (cons "C-{" "\\[")
                   (cons "C-}" "\\]")
                   (cons "," ",")
                   (cons "-" "-")
                   (cons ":" ":")
                   (cons ")" ")")
                   (cons "}" "}")
                   (cons "]" "]")))
    (define-key corfu-map (kbd (car c)) `(lambda ()
                                         (interactive)
                                         #'(corfu-quit)
                                         (insert ,(cdr c)))))

  (corfu-history-mode)
  (general-with 'savehist
    (general-pushnew 'corfu-history savehist-additional-variables))

  (general-def :keymaps ju//minibuffer-maps
    "<escape>" (defun corfu-quit-minibuffer ()
                 "`escape-quit-minibuffer' but quit corfu if active."
                 (interactive)
                 (when (and (boundp 'corfu--frame)
                            (frame-live-p corfu--frame))

                   (corfu-quit))
                 (keyboard-escape-quit))))

(use-package prescient)

(use-package corfu-prescient
  :ensure t
  :after prescient
  :config
  (corfu-prescient-mode 1))

(use-package orderless
  :init
  (setq completion-category-defaults nil
        ;; keep basic as fallback "to ensure that completion commands which
        ;; rely on dynamic completion tables work correctly"
        completion-styles '(orderless basic)
        ;; necessary for tramp hostname completion when using orderless
        completion-category-overrides
        '((file (styles basic partial-completion))))

  :config
  (defvar ju/orderless--separator "[ &]")

  (defun orderless-fast-dispatch (word index total)
    (and (= index 0) (= total 1) (length< word 1)
         `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

  (orderless-define-completion-style orderless-fast
    (orderless-style-dispatchers '(orderless-fast-dispatch))
    (orderless-matching-styles '(orderless-literal orderless-regexp)))

(setq vertico-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8)))

  (setq orderless-matching-styles
        '(orderless-literal
          orderless-prefixes
          orderless-initialism
          orderless-regexp)))

(use-package consult
  :elpaca (consult :files (:defaults "consult-*"))
  :general
  (leader
    "SPC" #'switch-to-buffer)
  (leader/system
    "s" #'switch-to-buffer)
  ("C-s"     #'consult-line
   "M-s"     #'consult-buffer
   "C-x C-r" #'consult-recent-file)

  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  :config
  (wm-map "M-y" #'consult-yank-from-kill-ring)

  (defun my/consult-line-forward ()
    "Search for a matching line forward."
    (interactive)
    (consult-line))

(defun my/consult-line-backward ()
  "Search for a matching line backward."
  (interactive)
  (advice-add 'consult--line-candidates :filter-return 'reverse)
  (vertico-reverse-mode +1)
  (unwind-protect (consult-line)
    (vertico-reverse-mode -1)
    (advice-remove 'consult--line-candidates 'reverse)))

(with-eval-after-load 'consult
  (consult-customize my/consult-line-backward
                     :prompt "Go to line backward: ")
  (consult-customize my/consult-line-forward
                     :prompt "Go to line forward: "))

(global-set-key (kbd "C-s") 'my/consult-line-forward)
(global-set-key (kbd "C-r") 'my/consult-line-backward)

(defun define-minibuffer-key (key &rest defs)
  "Define KEY conditionally in the minibuffer.
DEFS is a plist associating completion categories to commands."
  (define-key minibuffer-local-map key
    (list 'menu-item nil defs :filter
          (lambda (d)
            (plist-get d (completion-metadata-get
                          (completion-metadata (minibuffer-contents)
                                               minibuffer-completion-table
                                               minibuffer-completion-predicate)
                          'category))))))

(define-minibuffer-key "\C-s"
  'consult-location #'previous-history-element
  'file #'consult-find-for-minibuffer)

(defun consult-find-for-minibuffer ()
  "Search file with find, enter the result in the minibuffer."
  (interactive)
  (require 'consult)
  (let* ((enable-recursive-minibuffers t)
         (default-directory (file-name-directory (minibuffer-contents)))
         (file (consult--find
                (replace-regexp-in-string
                 "\\s-*[:([].*"
                 (format " (via find in %s): " default-directory)
                 (minibuffer-prompt))
                (consult--find-make-builder)
                (file-name-nondirectory (minibuffer-contents)))))
    (delete-minibuffer-contents)
    (insert (expand-file-name file default-directory))
    (exit-minibuffer))))

(use-package consult-gh ; Consult Github
  :elpaca (:host github :repo "armindarvish/consult-gh"))

(use-package vertico-prescient
  :after vertico
  :demand t
  :config
  (setq vertico-prescient-enable-filtering nil)
  (vertico-prescient-mode))

(use-package vertico-posframe
  :after vertico
  :config
  (vertico-posframe-mode)
  (gsetq posframe-mouse-banish-function #'posframe-mouse-banish-simple))

(use-package vertico
  :ensure t
  :defer 1
  :general
  (nmap "?" #'vertico-repeat)
  (general-def 'vertico-map
    "." #'vertico-repeat-last
    "," #'vertico-repeat-select
    "C-l" #'vertico-directory-enter
    "C-j" #'vertico-next
    "C-h" (cmds! (eq 'file (vertico--metadata-get 'category)) #'vertico-directory-up)
    "C-k" #'vertico-previous
    "C-i" #'vertico-insert
    "C-o" #'vertico-first
    "C-u" #'vertico-scroll-down
    "C-d" #'vertico-scroll-up
    "<tab>" #'vertico-insert
    "<next>" #'vertico-scroll-up
    "<prior>" #'vertico-scroll-down
    "<escape>" #'escape
    "<return>" #'vertico-directory-enter
    "<backspace>" #'vertico-directory-delete-char
    "C-<backspace>" #'vertico-directory-delete-word)

  :init
  (vertico-mode)
  (vertico-mouse-mode)
  (vertico-indexed-mode)
  :hook
  ((minibuffer-setup-hook . #'vertico-repeat-save))
  :config
  (vertico-multiform-mode)
  (gsetq vertico-buffer-display-action '(display-buffer-reuse-window)) ; Default
  (gsetq vertico-sort-function #'sort-directories-first)

  (gsetq vertico-multiform-commands
        '((describe-symbol (vertico-sort-function . vertico-sort-alpha))))
  (gsetq vertico-multiform-categories
         '((symbol (vertico-sort-function . vertico-sort-alpha))
           (consult-grep buffer)))

  (defadvice vertico-insert
      (after vertico-insert-add-history activate)
    "Make vertico-insert add to the minibuffer history."
    (unless (eq minibuffer-history-variable t)
      (add-to-history minibuffer-history-variable (minibuffer-contents))))

  (add-hook 'minibuffer-setup #'vertico-repeat-save) ; Make sure vertico state is saved
  (add-hook 'rfn-eshadow-update-overlay #'vertico-directory-tidy) ; Correct file path when changed

  (general-pushnew 'vertico-repeat-history savehist-additional-variables)

  (gsetq vertico-count 10
        vertico-scroll-margin 8
        vertico-resize nil
        vertico-cycle nil)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  (gsetq vertico-indexed-start 1)

  (dolist (c (list (cons "SPC" " ")
                   (cons "." ".")
                   (cons "C-1" "1")
                   (cons "C-2" "2")
                   (cons "C-3" "3")
                   (cons "C-4" "4")
                   (cons "C-5" "5")
                   (cons "C-6" "6")
                   (cons "C-7" "7")
                   (cons "C-8" "8")
                   (cons "C-9" "9")
                   (cons "C-0" "0")
                   (cons "C-(" "\\(")
                   (cons "C-)" "\\)")
                   (cons "C-{" "\\[")
                   (cons "C-}" "\\]")
                   (cons "," ",")
                   (cons "=" "=")
                   (cons ":" ":")
                   (cons ")" ")")
                   (cons "}" "}")
                   (cons "]" "]")))
    (define-key vertico-map (kbd (car c)) `(lambda ()
                                           (interactive)
                                           (insert ,(cdr c)))))

  (defmacro generate-vertico-select-index! (index)
    "Return a named function to run `vertico-directory-enter' for INDEX."
    `(defun! ,(intern (format "noct-vertico-enter %s" index)) ()
                 ,(format "Call `vertico-directory-enter' for index %s." index)
                 (interactive)
                 (let ((vertico--index ,index))
                   (vertico-directory-enter))))

  (general-def vertico-map
    "<prior>" #'vertico-scroll-down
    "<next>" #'vertico-scroll-up
    "1" (generate-vertico-select-index! 0)
    "2" (generate-vertico-select-index! 1)
    "3" (generate-vertico-select-index! 2)
    "4" (generate-vertico-select-index! 3)
    "5" (generate-vertico-select-index! 4)
    "6" (generate-vertico-select-index! 5)
    "7" (generate-vertico-select-index! 6)
    "8" (generate-vertico-select-index! 7)
    "9" (generate-vertico-select-index! 8)
    "0" (generate-vertico-select-index! 9))

  ;; Sort directories before files
  (defun sort-directories-first (files)
    (setq files (vertico-sort-history-length-alpha files))
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))

  ;; Prefix the current candidate with “» ”. From
  ;; https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand))))

(use-package embark
  :ensure t
  :general
  ("C-;" #'embark-dwim
   "C-." #'embark-act)
  (:keymaps ju//minibuffer-maps
            "C-]" #'embark-act
            "C-;" #'embark-dwim
            "C-h B" #'embark-bindings)
  :config
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                 (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

(setq prefix-help-command #'embark-prefix-help-command)
(add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target))
;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package marginalia
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'left)
  (marginalia-align-offset 20)
  :init
  (marginalia-mode)
  (gsetq marginalia-annotators
         '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(provide 'completion)
;; completion.el ends here
