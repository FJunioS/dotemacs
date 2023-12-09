;;; completion.el ---  desc  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'core-packages)

;; Configure Tempel
(use-package tempel
  :ensure t
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))
  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  (csetq tempel-template-sources 'tempel-path-templates
         tempel-trigger-prefix ""
         tempel-path (concat emacs-dir "templates"))
  (create-keymap snippet)
  (map leader-map "t" snippet-map)
  (map snippet-map
       "t" #'tempel-insert
       "c" #'tempel-complete)

  (defun tempel-include (elt)
    (when (eq (car-safe elt) 'i)
      (if-let (template (alist-get (cadr elt) (tempel--templates)))
          (cons 'l template)
        (message "Template %s not found" (cadr elt))
        nil)))
  (add-to-list 'tempel-user-elements #'tempel-include)
  )


;; (use-package transducers)

(use-package spacious-padding
  :init
  (when (require 'spacious-padding nil t)
    (csetq spacious-padding-widths
          '( :internal-border-width 15
             :header-line-width 4
             :mode-line-width 6
             :tab-width 4
             :right-divider-width 30
             :scroll-bar-width 8))
    (spacious-padding-mode 1)))

(use-package cape
  :ensure t
  :init
  (pushnew! completion-at-point-functions
            #'cape-file
            #'cape-abbrev
            #'cape-history))

(use-package corfu
  :elpaca (corfu :files (:defaults "extensions/*.el"))
  :ghook
  'prog-mode-hook
  'shell-mode-hook
  'eshell-mode-hook
  'eglot-managed-mode-hook
  :config
  (corfu-popupinfo-mode)
  (corfu-indexed-mode)

  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)      (corfu-mode 1)))

  (dolist (c '(minibuffer-setup-hook eshell-mode-hook))
    (add-hook c #'corfu-enable-in-minibuffer))

  (setq corfu-auto t
        corfu-preview-current nil
        corfu-auto-delay 0.15
        corfu-quit-no-match t
        corfu-indexed-start 1
        corfu-auto-prefix 3)

  (dolist (c (list (cons "SPC" " ")
                   (cons "." ".")
                   (cons "C-(" "\\(")
                   (cons "C-)" "\\)")
                   (cons "C-[" "\\[")
                   (cons "C-]" "\\]")
                   (cons "=" "=")
                   (cons ":" ":")
                   (cons "C-1" "(")
                   (cons "C-7" "[")
                   (cons "C-5" "{")
                   (cons "C-3" "}")
                   (cons "C-2" ")")
                   (cons "C-]" "]")
                   (cons "1" "1")
                   (cons "2" "2")
                   (cons "3" "3")
                   (cons "4" "4")
                   (cons "5" "5")
                   (cons "6" "6")
                   (cons "7" "7")
                   (cons "8" "8")
                   (cons "9" "9")
                   (cons "0" "0")
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

  (cl-pushnew 'corfu-history savehist-additional-variables :test 'equal)

  (defmacro generate-corfu-select-index! (index)
    "return a named function to run `corfu-complete' for index."
    `(defun! ,(intern (format "ju-corfu-enter-index-%s" index)) ()
       ,(format "call `corfu-complete' for index %s." index)
       (interactive)
       (let ((corfu--index ,index))
         (corfu-complete))))

  (map corfu-map
       "C-SPC" #'corfu-insert-separator
       "C-h" #'corfu--popup-show
       "C-t" #'corfu-insert
       "C-n" #'corfu-next
       "C-p" #'corfu-previous
       "C-l" #'corfu-complete
       "C-u" #'corfu-scroll-down
       "C-d" #'corfu-scroll-up
       "<tab>" #'corfu-complete

       "<return>" #'(lambda () (interactive)
                 (corfu-complete)
                 (call-interactively #'newline))

       "<escape>" (defun corfu-quit-minibuffer ()
                    "`escape-quit-minibuffer' but quit corfu if active."
                    (interactive)
                    (when (and (boundp 'corfu--frame)
                               (frame-live-p corfu--frame))
                      (corfu-quit))
                    (keyboard-quit))
       "(" (generate-corfu-select-index! 0)
       ")" (generate-corfu-select-index! 1)
       "}" (generate-corfu-select-index! 2)
       "+" (generate-corfu-select-index! 3)
       "{" (generate-corfu-select-index! 4)
       "]" (generate-corfu-select-index! 5)
       "[" (generate-corfu-select-index! 6)
       "!" (generate-corfu-select-index! 7)
       "=" (generate-corfu-select-index! 8)
       "*" (generate-corfu-select-index! 9)))

(use-package prescient)

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
  :defer 0

  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  :config
  (map leader-map "SPC" #'switch-to-buffer)
  (global-map
   "C-s"     #'consult-line
   "M-s"     #'consult-buffer
   "C-x C-r" #'consult-recent-file)

  (with-eval-after-load 'exwm
    (wm-map "M-y" #'consult-yank-from-kill-ring))

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

  (consult-customize my/consult-line-backward
                     :prompt "Go to line backward: ")
  (consult-customize my/consult-line-forward
                     :prompt "Go to line forward: ")

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
  :after consult
  :ensure t
  :elpaca (:host github :repo "armindarvish/consult-gh"))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

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
  (csetq posframe-mouse-banish-function #'posframe-mouse-banish-simple))

(use-package vertico
  :ensure t
  :hook ((elpaca-after-init-hook . vertico-mode)
         (minibuffer-setup-hook  . vertico-repeat-save))
  :init
  (when (require 'vertico nil t)
    (vertico-mode 1)
    (vertico-mouse-mode 1)
    (vertico-multiform-mode 1)
    (vertico-indexed-mode 1)
    (cl-pushnew 'vertico-repeat-history savehist-additional-variables))
  :config
  (csetq vertico-count 10
         vertico-scroll-margin 8
         vertico-resize nil
         vertico-cycle nil
         vertico-indexed-start 1)

  (csetq vertico-buffer-display-action '(display-buffer-reuse-window)) ; Default
  (csetq vertico-sort-function #'sort-directories-first)
  (csetq vertico-multiform-commands
         '((describe-symbol (vertico-sort-function . vertico-sort-alpha))))
  (csetq vertico-multiform-categories
         '((symbol (vertico-sort-function . vertico-sort-alpha))
           (consult-grep buffer)))

  (defadvice vertico-insert
      (after vertico-insert-add-history activate)
    "Make vertico-insert add to the minibuffer history."
    (unless (eq minibuffer-history-variable t)
      (add-to-history minibuffer-history-variable (minibuffer-contents))))

  ;; Make sure vertico state is saved
  ;; so commands `vertico-repeat-last' and `vertico-repeat-select' can be useful.
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  ;; Clear input when inserting a new path with `/' or `~/'
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  (csetq-default read-extended-command-predicaten
                 #'command-completion-default-include-p)

  (dolist (c (list (cons "SPC" " ")
                   (cons "." ".")
                   (cons "C-(" "\\(")
                   (cons "C-)" "\\)")
                   (cons "C-[" "\\[")
                   (cons "C-]" "\\]")
                   (cons "=" "=")
                   (cons ":" ":")
                   (cons "C-1" "(")
                   (cons "C-7" "[")
                   (cons "C-5" "{")
                   (cons "C-3" "}")
                   (cons "C-2" ")")
                   (cons "C-]" "]")))
    (define-key vertico-map (kbd (car c)) `(lambda ()
                                           (interactive)
                                           (insert ,(cdr c)))))

  (defmacro generate-vertico-select-index! (index)
    "Return a named function to run `vertico-enter' for INDEX."
    `(defun! ,(intern (format "vertico-enter-index-%s" index)) ()
       ,(format "Call `vertico-enter' for index %s." index)
       (let ((vertico--index ,index))
         (interactive)
         (vertico-directory-enter))))

  (map vertico-map
       ";" #'vertico-repeat-last
       "," #'vertico-repeat-select
       "C-l" #'vertico-directory-enter
       "C-j" #'vertico-next
       "C-h" (cmds! (eq 'file (vertico--metadata-get 'category)) #'vertico-directory-up)
       "C-k" #'vertico-previous
       "C-i" #'vertico-insert
       "C-o" #'vertico-first

       "<tab>" #'vertico-insert
       "<next>" #'vertico-scroll-up
       "<prior>" #'vertico-scroll-down
       "<escape>" #'escape
       "<return>" #'vertico-directory-enter
       "<backspace>" #'vertico-directory-delete-char
       "C-<backspace>" #'vertico-directory-delete-word
       "<prior>" #'vertico-scroll-down
       "<next>" #'vertico-scroll-up
       "(" (generate-vertico-select-index! 0)
       ")" (generate-vertico-select-index! 1)
       "}" (generate-vertico-select-index! 2)
       "+" (generate-vertico-select-index! 3)
       "{" (generate-vertico-select-index! 4)
       "]" (generate-vertico-select-index! 5)
       "[" (generate-vertico-select-index! 6)
       "!" (generate-vertico-select-index! 7)
       "=" (generate-vertico-select-index! 8)
       "*" (generate-vertico-select-index! 9))

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
  (eval-when-compile
    (defmacro my/embark-ace-action (fn)
      `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
         (interactive)
         (with-demoted-errors "%s"
           (require 'ace-window)
           (let ((aw-dispatch-always t))
             (aw-switch-to-window (aw-select nil))
             (call-interactively (symbol-function ',fn)))))))

  (define-key embark-file-map     (kbd "o") (my/embark-ace-action find-file))
  (define-key embark-buffer-map   (kbd "o") (my/embark-ace-action switch-to-buffer))
  (define-key embark-bookmark-map (kbd "o") (my/embark-ace-action bookmark-jump))

  (defun sudo-find-file (file)
  "Open FILE as root."
  (interactive "FOpen file as root: ")
  (when (file-writable-p file)
    (user-error "File is user writeable, aborting sudo"))
  (find-file (if (file-remote-p file)
                 (concat "/" (file-remote-p file 'method) ":"
                         (file-remote-p file 'user) "@" (file-remote-p file 'host)
                         "|sudo:root@"
                         (file-remote-p file 'host) ":" (file-remote-p file 'localname))
               (concat "/sudo:root@localhost:" file))))
  (define-key embark-file-map (kbd "S") 'sudo-find-file)

  (define-key embark-region-map (kbd "U") '0x0-dwim)
  )

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
  (csetq marginalia-annotators
         '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(provide 'completion)
;; completion.el ends here

;;; completion.el ends here
