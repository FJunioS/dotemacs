;; -*- lexical-binding: t; -*-
(require 'core-packages)

(use-package cape
  :ensure t
  :init
  (pushnew! completion-at-point-functions
            #'cape-file
            #'cape-abbrev
            #'cape-history))
(use-package corfu
  :ensure (corfu :files (:defaults "extensions/*.el"))
  :init
  (dolist (mode '(prog-mode-hook org-mode-hook shell-mode-hook eshell-mode-hook eglot-managed))
    (add-hook (+unquote mode) #'corfu-mode))
  :config
  (defun corfu-enable-in-minibuffer ()
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      ;; (setq-local corfu-auto nil) ;; Enable/disable auto completion
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  (dolist (c '(minibuffer-setup-hook eshell-mode-hook))
    (add-hook c #'corfu-enable-in-minibuffer))

  (csetq corfu-auto t ;; Popup appears automatically
         corfu-preview-current t ;; Show candidate on pointer
         corfu-quit-no-match nil
         corfu-indexed-start 1
         corfu-auto-delay 0.15
         corfu-auto-prefix 3)

  (dolist (c (list (cons "SPC" " ")
                   (cons "." ".")
                   (cons "C-(" "\\(")
                   (cons "C-)" "\\)")
                   (cons "C-[" "\\[")
                   (cons "C-]" "\\]")
                   (cons "=" "=")
                   (cons ":" ":")
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

  (pushnew! savehist-additional-variables 'corfu-history)

  ;; (defmacro generate-corfu-select-index! (index)
  ;;   "return a named function to run `corfu-complete' for index."
  ;;   `(defun! ,(intern (format "ju-corfu-enter-index-%s" index)) ()
  ;;      ,(format "call `corfu-complete' for index %s." index)
  ;;      (interactive)
  ;;      (let ((corfu--index ,index))
  ;;        (corfu-complete))))

  (map corfu-map
       "C-SPC" #'corfu-insert-separator
       "C-h" #'corfu--popup-show
       "C-t" #'corfu-insert
       "C-n" #'corfu-next
       "C-p" #'corfu-previous
       "C-l" #'corfu-complete
       "C-v" #'corfu-scroll-down
       "C-S-v" #'corfu-scroll-up
       "<tab>" #'corfu-complete

       "<return>" #'(lambda () (interactive)
                      (corfu-complete)
                      (call-interactively #'newline))

       "ESC" (defun corfu-quit-minibuffer ()
               "`escape-quit-minibuffer' but quit corfu if active."
               (interactive)
               (when (and (boundp 'corfu--frame)
                          (frame-live-p corfu--frame))
                 (corfu-quit))
               (keyboard-quit))))
(use-package orderless
  :ensure t
  :init
  (csetq completion-styles '(orderless partial-completion basic)
         completion-category-defaults nil
         completion-category-overrides '((file     (styles orderless partial-completion))
                                         ;; enable initialism by default for symbols
                                         (symbol   (styles orderless))
                                         (command  (styles orderless))
                                         (variable (styles orderless)))
         orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
         orderless-style-dispatchers (list #'+orderless-consult-dispatch
                                           #'orderless-affix-dispatch))
  :config
  (defvar ju/orderless--separator "[ &]")

  (setq orderless-matching-styles
        '(orderless-literal
          orderless-prefixes
          orderless-initialism
          orderless-regexp))

  :preface
  (defun orderless-fast-dispatch (word index total)
    (and (= index 0) (= total 1) (length< word 1)
         `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

  (defun +orderless-consult-dispatch (word _index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" word)
      `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--consult-suffix))))
     ;; File extensions
     ((and (or minibuffer-completing-file-name
               (derived-mode-p 'eshell-mode))
           (string-match-p "\\`\\.." word))
      `(orderless-regexp . ,(concat "\\." (substring word 1) (+orderless--consult-suffix)))))))
(use-package prescient)
(use-package sudo-utils)
(use-package spacious-padding
  :init
  (when (require 'spacious-padding nil t)
    (csetq spacious-padding-widths
           '( :internal-border-width 15
              :header-line-width 4
              :tab-width 4
              :right-divider-width 30
              :scroll-bar-width 8))
    (spacious-padding-mode 1)))
(use-package consult
  :ensure (consult :files (:defaults "consult-*"))
  :defer 0
  :init
  (advice-add #'register-preview :override #'consult-register-window)
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  :config
  (map leader-map "SPC" #'switch-to-buffer)
  (map ju-menu-map "C-r" #'projectile-recentf-files)
  (global-map
   "M-s o"   #'consult-org-heading
   "C-s"     #'consult-line
   "M-h"     #'consult-buffer
   "C-x C-r" #'consult-recent-file)

  ;; orderless configuration
  (defun +orderless--consult-suffix ()
    "Regexp which matches the end of string with Consult tofu support."
    (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
        (format "[%c-%c]*$"
                consult--tofu-char
                (+ consult--tofu-char consult--tofu-range -1))
      "$"))

  ;; Recognizes the following patterns:
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)



  ;; ----------------------------------------------------------
  ;; Use consult-ripgrep instead of project-find-regexp in project.el
  (keymap-substitute project-prefix-map #'project-find-regexp #'consult-ripgrep)

  (cl-nsubstitute-if
   '(consult-ripgrep "Find regexp")
   (pcase-lambda (`(,cmd _)) (eq cmd #'project-find-regexp))
   project-switch-commands)

  ;; Org Capture
  (defun consult-org-capture-target (scope)
    "Choose a capture target interactively.
This function returns a value suitable for use as the `target'
entry of `org-capture-templates'.  SCOPE is as in `org-map-entries'."
    (list 'function
          (lambda ()
            (let ((consult--read-config `((,this-command
                                           :prompt "Capture target: "
                                           :preview-key "M-."))))
              (set-buffer (save-window-excursion
                            (consult-org-heading nil scope)
                            (current-buffer)))))))

  (set 'org-capture-templates
       `(("c" "Consult..." entry ,(consult-org-capture-target 'agenda)
          "* TODO %?\n  %i" :prepend t)))

  (defun consult-org-capture ()
    (interactive)
    (org-capture nil "c"))

  ;; -----------------------
  ;; unique path for recentf
  (defun my-consult--source-recentf-items-uniq ()
    (let ((ht (consult--buffer-file-hash))
          file-name-handler-alist ;; No Tramp slowdown please.
          items)
      (dolist (file (my-recentf-list-uniq) (nreverse items))
        ;; Emacs 29 abbreviates file paths by default, see
        ;; `recentf-filename-handlers'.
        (unless (eq (aref (cdr file) 0) ?/)
          (setcdr file (expand-file-name (cdr file))))
        (unless (gethash (cdr file) ht)
          (push (propertize
                 (car file)
                 'multi-category `(file . ,(cdr file)))
                items)))))

  (plist-put consult--source-recent-file
             :items #'my-consult--source-recentf-items-uniq)

  (defun my-recentf-list-uniq ()
    (let* ((proposed (mapcar (lambda (f)
                               (cons (file-name-nondirectory f) f))
                             recentf-list))
           (recentf-uniq proposed)
           conflicts resol file)
      ;; collect conflicts
      (while proposed
        (setq file (pop proposed))
        (if (assoc (car file) conflicts)
            (push (cdr file) (cdr (assoc (car file) conflicts)))
          (if (assoc (car file) proposed)
              (push (list (car file) (cdr file)) conflicts))))
      ;; resolve conflicts
      (dolist (name conflicts)
        (let* ((files (mapcar (lambda (f)
                                ;; data structure:
                                ;; (file remaining-path curr-propos)
                                (list f
                                      (file-name-directory f)
                                      (file-name-nondirectory f)))
                              (cdr name)))
               (curr-step (mapcar (lambda (f)
                                    (file-name-nondirectory
                                     (directory-file-name (cadr f))))
                                  files)))
          ;; Quick check, if there are no duplicates, we are done.
          (if (eq (length curr-step) (length (seq-uniq curr-step)))
              (setq resol
                    (append resol
                            (mapcar (lambda (f)
                                      (cons (car f)
                                            (file-name-concat
                                             (file-name-nondirectory
                                              (directory-file-name (cadr f)))
                                             (file-name-nondirectory (car f)))))
                                    files)))
            (while files
              (let (files-remain)
                (dolist (file files)
                  (let ((curr-propos (caddr file))
                        (curr-part (file-name-nondirectory
                                    (directory-file-name (cadr file))))
                        (rest-path (file-name-directory
                                    (directory-file-name (cadr file))))
                        (curr-step
                         (mapcar (lambda (f)
                                   (file-name-nondirectory
                                    (directory-file-name (cadr f))))
                                 files)))
                    (cond ((length= (seq-uniq curr-step) 1)
                           ;; If all elements of curr-step are equal, we skip
                           ;; this path part.
                           (push (list (car file)
                                       rest-path
                                       curr-propos)
                                 files-remain))
                          ((member curr-part (cdr (member curr-part curr-step)))
                           ;; There is more than one curr-part in curr-step
                           ;; for this candidate.
                           (push (list (car file)
                                       rest-path
                                       (file-name-concat curr-part curr-propos))
                                 files-remain))
                          (t
                           ;; There is no repetition of curr-part in curr-step
                           ;; for this candidate.
                           (push (cons (car file)
                                       (file-name-concat curr-part curr-propos))
                                 resol)))))
                (setq files files-remain))))))
      ;; apply resolved conflicts
      (let (items)
        (dolist (file recentf-uniq (nreverse items))
          (let ((curr-resol (assoc (cdr file) resol)))
            (if curr-resol
                (push (cons (cdr curr-resol) (cdr file)) items)
              (push file items)))))))
  ;; -----------------------
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

  ;; ------------------------------------------------------
  (require 'cl-lib)
  (require 'dom)
  (require 'url-util)
  (require 'xml)

  (defun consult--xdg-recent-file-list ()
    "Get a list of recently used files on XDG-compliant systems.

This function extracts a list of files from the file
`recently-used.xbel' in the folder `xdg-data-home'.

For more information on this specification, see
https://www.freedesktop.org/wiki/Specifications/desktop-bookmark-spec/"
    (let ((data-file (expand-file-name "recently-used.xbel" (xdg-data-home)))
          (xml-parsing-func (if (libxml-available-p)
                                #'libxml-parse-xml-region
                              #'xml-parse-region)))
      (if (file-readable-p data-file)
          (delq nil
                (mapcar (lambda (bookmark-node)
                          (when-let ((local-path (string-remove-prefix
                                                  "file://"
                                                  (dom-attr bookmark-node 'href))))
                            (let ((full-file-name (decode-coding-string
                                                   (url-unhex-string local-path)
                                                   'utf-8)))
                              (when (file-exists-p full-file-name)
                                full-file-name))))
                        (nreverse (dom-by-tag (with-temp-buffer
                                                (insert-file-contents data-file)
                                                (funcall xml-parsing-func
                                                         (point-min)
                                                         (point-max)))
                                              'bookmark))))
        (message "consult: List of XDG recent files not found")
        '())))

  (defun consult--recent-system-files ()
    "Return a list of files recently used by the system."
    (cl-case system-type
      (gnu/linux
       (consult--xdg-recent-file-list))
      (t
       (message "consult-recent-file: \"%s\" currently unsupported"
                system-type)
       '())))

  (defcustom consult-include-system-recent-files t
    "Whether to include files used by other programs in `consult-recent-file'."
    :type 'boolean
    :group 'consult)

  (defun consult--recent-files-mixed-candidates ()
    "Return a list of files recently used by Emacs and the system.

These files are sorted by modification time, from most recent to least."
    (thread-last
      (consult--recent-system-files)
      (seq-filter #'recentf-include-p)
      (append (mapcar #'substring-no-properties recentf-list))
      delete-dups
      (consult--recent-files-sort)))

  (defun consult--recent-files-sort (file-list)
    "Sort the FILE-LIST by modification time, from most recent to least recent."
    (thread-last
      file-list
      ;; Use modification time, since getting file access time seems to count as
      ;; accessing the file, ruining future uses.
      (mapcar (lambda (f)
                (cons f (file-attribute-modification-time (file-attributes f)))))
      (seq-sort (pcase-lambda (`(,f1 . ,t1) `(,f2 . ,t2))
                  ;; Want existing, most recent, local files first.
                  (cond ((or (not (file-exists-p f1))
                             (file-remote-p f1))
                         nil)
                        ((or (not (file-exists-p f2))
                             (file-remote-p f2))
                         t)
                        (t (time-less-p t2 t1)))))
      (mapcar #'car)))

  ;;;###autoload
  (defun consult-recent-file ()
    "Find recent using `completing-read'."
    (interactive)
    (find-file
     (consult--read
      (or (mapcar #'abbreviate-file-name
                  recentf-list)
          (user-error "No recent files"))
      :prompt "Find recent file: "
      :sort nil
      :require-match t
      :category 'file
      :state (consult--file-preview)
      :history 'file-name-history)))

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
  :ensure (:host github :repo "armindarvish/consult-gh"))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package vertico
  :ensure t
  :hook ((elpaca-after-init-hook . vertico-mode)
         (minibuffer-setup-hook  . vertico-repeat-save))
  :init
  (vertico-mode 1)
  (vertico-mouse-mode 1)
  (vertico-multiform-mode 1)
  (vertico-indexed-mode 1)
  ;; (vertico-buffer-mode) ;; Not so seamslesly
  (pushnew! savehist-additional-variables 'vertico-repeat-history)
  :config
  (csetq vertico-count 10
         vertico-scroll-margin 8
         vertico-resize t
         vertico-cycle t
         vertico-indexed-start 1) ;; TODO Set this to 0 and change macro impl below

  ;; Vertico Buffer settings
  (when vertico-buffer-mode
    (csetq vertico-buffer-display-action
           '(display-buffer-at-bottom (window-height . 11)))
    (add-hook 'minibuffer-setup-hook (lambda () (setq mode-line-format nil))))

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
    (unless minibuffer-history-variable
      (add-to-history minibuffer-history-variable (minibuffer-contents))))

  ;; Make sure vertico state is saved
  ;; so commands `vertico-repeat-last' and `vertico-repeat-select' can be useful.
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

  ;; Clear input when inserting a new path with `/' or `~/'
  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

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

  ;; (defmacro generate-vertico-select-index! (index)
  ;;   "Return a named function to run `vertico-enter' for INDEX."
  ;;   `(defun! ,(intern (format "vertico-enter-index-%s" index)) ()
  ;;      ,(format "Call `vertico-enter' for index %s." index)
  ;;      (interactive)
  ;;      (let ((vertico--index ,index))
  ;;        (vertico-directory-enter))))

  (map vertico-map
       "ESC" nil ;; necessary to avoid prefix errors (TESTING)
       "C-l" #'vertico-repeat-last
       "C-," #'vertico-repeat-select
       "C-p" #'vertico-previous
       "C-n" #'vertico-next
       "C-h" (cmds! (eq 'file (vertico--metadata-get 'category)) #'vertico-directory-up)
       "C-o" #'vertico-first
       "C-w" #'vertico-save

       ;; Scrolling
       "C-v" (lambda () (interactive) (vertico-scroll-down -1))
       "M-v" (lambda () (interactive) (vertico-scroll-down 1))

       "<tab>" #'vertico-insert

       "<return>" #'vertico-directory-enter
       "M-<return>" #'vertico-exit-input ;; insert what is on input and close
       "<backspace>" #'vertico-directory-delete-char
       "C-<backspace>" #'vertico-directory-delete-word
       "<escape>" #'escape ;; Restore ESC behavior
       )

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
                 cand)))

  (defvar +vertico-transform-functions nil)

  (cl-defmethod vertico--format-candidate :around
    (cand prefix suffix index start &context ((not +vertico-transform-functions) null))
    (dolist (fun (ensure-list +vertico-transform-functions))
      (setq cand (funcall fun cand)))
    (cl-call-next-method cand prefix suffix index start))

  (defun +vertico-highlight-directory (file)
    "If FILE ends with a slash, highlight it as a directory."
    (if (string-suffix-p "/" file)
        (propertize file 'face 'marginalia-file-priv-dir) ; or face 'dired-directory
      file))

  ;; function to highlight enabled modes similar to counsel-M-x
  (defun +vertico-highlight-enabled-mode (cmd)
    "If MODE is enabled, highlight it as font-lock-constant-face."
    (let ((sym (intern cmd)))
      (if (or (eq sym major-mode)
              (and
               (memq sym minor-mode-list)
               (boundp sym)))
          (propertize cmd 'face 'font-lock-constant-face)
        cmd)))

  ;; add-to-list works if 'file isn't already in the alist
  ;; setq can be used but will overwrite all existing values
  (add-to-list 'vertico-multiform-categories
               '(file
                 ;; this is also defined in the wiki, uncomment if used
                 ;; (vertico-sort-function . sort-directories-first)
                 (+vertico-transform-functions . +vertico-highlight-directory))))

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

(use-package embark
  :ensure t

  :config
  (map global-map
       "C-;" #'embark-dwim
       "C-." #'embark-act)
  (dolist (m  ju//minibuffer-maps)
    (map (symbol-value m)
         "C-SPC" #'embark-select
         "C-." #'embark-act
         "C-;" #'embark-dwim
         "C-h B" #'embark-bindings))
  (setq embark-quit-after-action '((kill-buffer . nil) (t . nil)))
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

  (define-key embark-region-map (kbd "U") '0x0-dwim))

(use-package embark-consult
  :ensure t ; only need to install it, embark loads it after consult if found
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))
(use-package marginalia
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'left)
  (marginalia-align-offset 10)
  :init
  (marginalia-mode)
  (csetq marginalia-annotators
         '(marginalia-annotators-heavy marginalia-annotators-light nil)))

(provide 'completion)
;;; completion.el ends here


(provide 'completion)
;;; completion.el ends here.
