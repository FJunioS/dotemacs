;;; core-lib.el --- Core standard library -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'cl-lib)

(define-error 'config-error "An unexpected config error")
(define-error 'core-error "Unexpected error in Emacs Config's CORE layer" 'config-error)
(define-error 'hook-error "Error in a Config startup hook" 'config-error)
(define-error 'autoload-error "Error in config's autoloads file" 'config-error)
(define-error 'user-error "Error caused by user's config or system" 'config-error)
(define-error 'module-error "Error in a config module" 'config-error)
(define-error 'package-error "Error with packages" 'config-error)
(define-error 'cache-error "Error while processing cache" 'config-error)

(defvar +inhibit-log (not (or noninteractive init-file-debug))
  "If non-nil, suppress `+log' output.")

(defun __log (text &rest args)
  (let ((inhibit-message (not init-file-debug))
        (absolute? (string-prefix-p ":" text)))
    (apply #'message
           (propertize (concat "* %.06f:%s" (if (not absolute?) ":") text)
                       'face 'font-lock-doc-face)
           (float-time (time-subtract (current-time) before-init-time))
           (mapconcat
            (lambda (x) (format "%s" x))
            (unless absolute?
              (append (cons '* (remq t (reverse +context)))
                      (if (bound-and-true-p +module-context)
                          (let ((key (+module-context-key)))
                            (delq nil (list (car key) (cdr key)))))))
            ":")
           args)))

(defmacro +log (message &rest args)
  "Log a message in *Messages*.

Does not emit the MESSAGE in the echo area.
This is a macro instead of a function to prevent the potentially expensive
evaluation of its arguments when debug mode is off.
Return ARGS non-nil."
  (declare (debug t))
  `(unless +inhibit-log (__log ,message ,@args)))

(defalias '+partial #'apply-partially)

(defmacro null-or! (&rest body)
  `(satisfies (lambda (x) (or (null x) (,@body)))))

(defmacro after! (package &rest body)
  "Evaluate BODY after PACKAGE have loaded.

PACKAGE is a symbol (or list of them) referring to Emacs features (aka
packages). PACKAGE may use :or/:any and :and/:all operators. The precise format
is:

- An unquoted package symbol (the name of a package)
    (after! helm BODY...)
- An unquoted, nested list of compound package lists, using any combination of
  :or/:any and :and/:all
    (after! (:or package-a package-b ...)  BODY...)
    (after! (:and package-a package-b ...) BODY...)
    (after! (:and package-a (:or package-b package-c) ...) BODY...)
- An unquoted list of package symbols (i.e. BODY is evaluated once both magit
  and git-gutter have loaded)
    (after! (magit git-gutter) BODY...)
  If :or/:any/:and/:all are omitted, :and/:all are implied.

This emulates `eval-after-load' with a few key differences:

1. Supports compound package statements (see :or/:any and :and/:all above).

Since the contents of these blocks will never by byte-compiled, avoid putting
things you want byte-compiled in them! Like function/macro definitions."
  (declare (indent defun) (debug t))
  (if (symbolp package)
      (unless (memq package (bound-and-true-p doom-disabled-packages))
        (list (if (or (not (bound-and-true-p byte-compile-current-file))
                      (require package nil 'noerror))
                  #'progn
                #'with-no-warnings)
              `(with-eval-after-load ',package ,@body)))
    (let ((p (car package)))
      (cond ((memq p '(:or :any))
             (macroexp-progn
              (cl-loop for next in (cdr package)
                       collect `(after! ,next ,@body))))
            ((memq p '(:and :all))
             (dolist (next (reverse (cdr package)) (car body))
               (setq body `((after! ,next ,@body)))))
            (`(after! (:and ,@package) ,@body))))))

(defun load! (path &optional noerror)
  "Load PATH and handle any config errors that may arise from it.

If NOERROR, don't throw an error if PATH doesn't exist."
  (+log "load: %s %s" (abbreviate-file-name path) noerror)
  (condition-case-unless-debug e
      (load path noerror 'nomessage)
    (config-error
     (signal (car e) (cdr e)))
    (error
     (setq path (locate-file path load-path (get-load-suffixes)))
     (signal (cond ((not (and path (featurep 'config)))
                    'error)
                   ((file-in-directory-p path core-dir)
                    'core-error)
                   ((file-in-directory-p path user-dir)
                    'user-error)
                   ((file-in-directory-p path cache-dir)
                    'cache-error)
                   ((file-in-directory-p path layers-dir)
                    'module-error)
                   ('config-error))
             (list path e)))))

(defun require! (feature &optional filename noerror)
  "Evaluate FEATURE, just like `require', but handles and enhances errors.

Can also load subfeatures, e.g. (require! '+some-lib 'some-files)"
  (let ((subfeature (if (symbolp filename) filename)))
    (or (featurep feature subfeature)
        (load!
         (if subfeature
             (file-name-concat core-dir
                               (string-remove-prefix "+" (symbol-name feature))
                               (symbol-name filename))
           (symbol-name feature))
         noerror))))


(defun __resolve-hook-forms (hooks)
  "Converts a list of modes into a list of hook symbols.

If a mode is quoted, it is left as is. If the entire HOOKS list is quoted, the
list is returned as-is."
  (declare (pure t) (side-effect-free t))
  (let ((hook-list (ensure-list (+unquote hooks))))
    (if (eq (car-safe hooks) 'quote)
        hook-list
      (cl-loop for hook in hook-list
               if (eq (car-safe hook) 'quote)
               collect (cadr hook)
               else collect (intern (format "%s-hook" (symbol-name hook)))))))

(defmacro letf! (bindings &rest body)
  "Temporarily rebind function, macros, and advice in BODY.

Intended as syntax sugar for `cl-letf', `cl-labels', `cl-macrolet', and
temporary advice.

BINDINGS is either:

  A list of, or a single, `defun', `defun*', `defmacro', or `defadvice' forms.
  A list of (PLACE VALUE) bindings as `cl-letf*' would accept.

TYPE is one of:

  `defun' (uses `cl-letf')
  `defun*' (uses `cl-labels'; allows recursive references),
  `defmacro' (uses `cl-macrolet')
  `defadvice' (uses `defadvice!' before BODY, then `undefadvice!' after)

NAME, ARGLIST, and BODY are the same as `defun', `defun*', `defmacro', and
`defadvice!', respectively.

\(fn ((TYPE NAME ARGLIST &rest BODY) ...) BODY...)"
  (declare (indent defun))
  (setq body (macroexp-progn body))
  (when (memq (car bindings) '(defun defun* defmacro defadvice))
    (setq bindings (list bindings)))
  (dolist (binding (reverse bindings) body)
    (let ((type (car binding))
          (rest (cdr binding)))
      (setq
       body (pcase type
              (`defmacro `(cl-macrolet ((,@rest)) ,body))
              (`defadvice `(progn (defadvice! ,@rest)
                                  (unwind-protect ,body (undefadvice! ,@rest))))
              ((or `defun `defun*)
               `(cl-letf ((,(car rest) (symbol-function #',(car rest))))
                  (ignore ,(car rest))
                  ,(if (eq type 'defun*)
                       `(cl-labels ((,@rest)) ,body)
                     `(cl-letf (((symbol-function #',(car rest))
                                 (lambda! ,(cadr rest) ,@(cddr rest))))
                        ,body))))
              (_
               (when (eq (car-safe type) 'function)
                 (setq type (list 'symbol-function type)))
               (list 'cl-letf (list (cons type rest)) body)))))))

(defun +unquote (exp)
  "Return EXP unquoted."
  (declare (pure t) (side-effect-free t))
  (while (memq (car-safe exp) '(quote function))
    (setq exp (cadr exp)))
  exp)

(defun +keyword-intern (str)
  "Converts STR (a string) into a keyword (`keywordp')."
  (declare (pure t) (side-effect-free t))
  (cl-check-type str string)
  (intern (concat ":" str)))

(defun +keyword-name (keyword)
  "Returns the string name of KEYWORD (`keywordp') minus the leading colon."
  (declare (pure t) (side-effect-free t))
  (cl-check-type keyword keyword)
  (substring (symbol-name keyword) 1))

(defun +rpartial (fn &rest args)
  "Return a partial application of FUN to right-hand ARGS.

ARGS is a list of the last N arguments to pass to FUN. The result is a new
function which does the same as FUN, except that the last N arguments are fixed
at the values with which this function was called."
  (declare (side-effect-free t))
  (lambda (&rest pre-args)
    (apply fn (append pre-args args))))

(defun +lookup-key (keys &rest keymaps)
  "Like `lookup-key', but search active keymaps if KEYMAP is omitted."
  (if keymaps
      (cl-some (+rpartial #'lookup-key keys) keymaps)
    (cl-loop for keymap
             in (append (cl-loop for alist in emulation-mode-map-alists
                                 append (mapcar #'cdr
                                                (if (symbolp alist)
                                                    (if (boundp alist) (symbol-value alist))
                                                  alist)))
                        (list (current-local-map))
                        (mapcar #'cdr minor-mode-overriding-map-alist)
                        (mapcar #'cdr minor-mode-map-alist)
                        (list (current-global-map)))
             if (keymapp keymap)
             if (lookup-key keymap keys)
             return it)))

(defmacro add-hook! (hooks &rest rest)
  "A convenience macro for adding N functions to M hooks.

This macro accepts, in order:

  1. The mode(s) or hook(s) to add to. This is either an unquoted mode, an
    unquoted list of modes, a quoted hook variable or a quoted list of hook
    variables.
  2. Optional properties :local, :append, and/or :depth [N], which will make the
    hook buffer-local or append to the list of hooks (respectively),
  3. The function(s) to be added: this can be a quoted function, a quoted list
    thereof, a list of `defun' or `cl-defun' forms, or arbitrary forms (will
    implicitly be wrapped in a lambda).

\(fn HOOKS [:append :local [:depth N]] FUNCTIONS-OR-FORMS...)"
  (declare (indent (lambda (indent-point state)
                     (goto-char indent-point)
                     (when (looking-at-p "\\s-*(")
                       (lisp-indent-defform state indent-point))))
           (debug t))
  (let* ((hook-forms (__resolve-hook-forms hooks))
         (func-forms ())
         (defn-forms ())
         append-p local-p remove-p depth)
    (while (keywordp (car rest))
      (pcase (pop rest)
        (:append (setq append-p t))
        (:depth  (setq depth (pop rest)))
        (:local  (setq local-p t))
        (:remove (setq remove-p t))))
    (while rest
      (let* ((next (pop rest))
             (first (car-safe next)))
        (push (cond ((memq first '(function nil))
                     next)
                    ((eq first 'quote)
                     (let ((quoted (cadr next)))
                       (if (atom quoted)
                           next
                         (when (cdr quoted)
                           (setq rest (cons (list first (cdr quoted)) rest)))
                         (list first (car quoted)))))
                    ((memq first '(defun cl-defun))
                     (push next defn-forms)
                     (list 'function (cadr next)))
                    ((prog1 `(lambda (&rest _) ,@(cons next rest))
                       (setq rest nil))))
              func-forms)))
    `(progn
       ,@defn-forms
       (dolist (hook (nreverse ',hook-forms))
         (dolist (func (list ,@func-forms))
           ,(if remove-p
                `(remove-hook hook func ,local-p)
              `(add-hook hook func ,(or depth append-p) ,local-p)))))))


(defmacro unsetq-hook! (hooks &rest vars)
  "Unbind setq hooks on HOOKS for VARS.

\(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (_var _val hook fn)
            in (doom--setq-hook-fns hooks vars 'singles)
            collect `(remove-hook ',hook #',fn))))


;;; Definers
(defmacro defadvice! (symbol arglist &optional docstring &rest body)
  "Define an advice called SYMBOL and add it to PLACES.

ARGLIST is as in `defun'. WHERE is a keyword as passed to `advice-add', and
PLACE is the function to which to add the advice, like in `advice-add'.
DOCSTRING and BODY are as in `defun'.

\(fn SYMBOL ARGLIST &optional DOCSTRING &rest [WHERE PLACES...] BODY\)"
  (declare (doc-string 3) (indent defun))
  (unless (stringp docstring)
    (push docstring body)
    (setq docstring nil))
  (let (where-alist)
    (while (keywordp (car body))
      (push `(cons ,(pop body) (ensure-list ,(pop body)))
            where-alist))
    `(progn
       (defun ,symbol ,arglist ,docstring ,@body)
       (dolist (targets (list ,@(nreverse where-alist)))
         (dolist (target (cdr targets))
           (advice-add target (car targets) #',symbol))))))



(defvar __hook nil)
(defun +run-hook (hook)
  "Run HOOK (a hook function) with better error handling.
Meant to be used with `run-hook-wrapped'."
  (+log "hook:%s: run %s" (or __hook '*) hook)
  (condition-case-unless-debug e
      (funcall hook)
    (error
     (signal 'hook-error (list hook e))))
  ;; return nil so `run-hook-wrapped' won't short circuit
  nil)


(defun +default-monitor-geometry ()
  "Return geometry for the first monitor in `display-monitor-attributes-list'."
  (let* ((first-monitor (car (display-monitor-attributes-list))))
    (alist-get 'geometry first-monitor)))

(defun +default-monitor-width ()
  "Return the width of the first monitor in `display-monitor-attributes-list'."
  (nth 2 (+default-monitor-geometry)))

(defun +default-monitor-height ()
  "Return the height of the first monitor in `display-monitor-attributes-list'."
  (nth 3 (+default-monitor-geometry)))


(defun +border-width ()
  "Return the width to use for borders.
Uses 4 pixels FHD and 8 on 4k."
  (round (* 0.00208333333 (+default-monitor-width))))

(defun +mode-line-height ()
  (round (* 0.00911458333333333 (+default-monitor-width))))

(defun +smaller-mode-line-height ()
  (round (* 0.0078125 (+default-monitor-width))))

(defun vsplit ()
  "Vertically split window and switch to new window."
  (interactive)
  (split-window-below)
  (other-window 1)
  (balance-windows))

(defun hsplit ()
  "Horizontally split window and switch to new window."
  (interactive)
  (split-window-right)
  (other-window 1)
  (balance-windows))

(defun +switch-to-messages ()
  (interactive)
  (switch-to-buffer "*Messages*"))

;;;###autoload
(defmacro csetq (&rest settings)
    `(progn
     ,@(cl-loop for (var val) on settings by 'cddr
                collect `(funcall (or (get ',var 'custom-set) #'set)
                                  ',var ,val))))

(defmacro csetq-default (&rest settings)
  "An alias for `setq-default'."
  `(setq-default ,@settings))

(defmacro create-keymap (key)
  "Creates a keymap KEY."
  (let ((k (intern (concat (symbol-name key) "-map"))))
    `(progn
       (defvar ,k (make-sparse-keymap) ,(concat (symbol-name key) "-map."))
       (defalias ',key ,k))))

(defmacro map (mode &rest keymaps)
  `(progn
     ,@(cl-loop for (hk fn) on keymaps by 'cddr
                collect `(funcall #'define-key ,mode ,(kbd hk) ,(if (stringp fn) (kbd fn) fn)))))

(defmacro global-map (&rest keymaps)
  `(progn
     ,@(cl-loop for (hk fn) on keymaps by 'cddr
                collect `(funcall #'global-set-key ,(kbd hk) ,(if (stringp fn) (kbd fn) fn)))))
(defalias #'global-key #'global-map)

(defmacro +map (map &rest bindings)
  `(progn
     ,@(seq-map (lambda (pair)
                  `(define-key! ,map (kbd ,(car pair)) ,(cadr pair)))
                (seq-partition bindings 2))))

(defmacro define-key! (map keybind command)
  `(define-key ,map (kbd ,keybind) ',command))

(defmacro defun! (name arglist &optional docstring &rest body)
  "declare `defun' and return the function instead of a unit."
  (declare (doc-string 3) (indent 2))
  `(progn (defun ,name ,arglist ,docstring ,@body)
          #',name))

(defmacro pushnew! (place &rest values)
  "Push VALUES sequentially into PLACE, if they aren't already present.
This is a variadic `cl-pushnew'."
  (let ((var (make-symbol "result")))
    `(dolist (,var (list ,@values) (with-no-warnings ,place))
       (cl-pushnew ,var ,place :test #'equal))))

(defmacro run-at-interval! (interval idle-interval &rest body)
  "Every INTERVAL seconds, unless idle for > IDLE-INTERVAL seconds, run BODY.
    Also, after IDLE-INTERVAL seconds of idle time, run BODY. This allows using an
    idle timer to quickly run BODY when Emacs becomes idle but also ensures that
    BODY is run periodically even if Emacs is actively being used."
  (declare (indent 2))
  `(progn
     (run-at-time (current-time) ,interval
                  (lambda ()
                    (let* ((idle-time (current-idle-time))
                           (idle-secs (when idle-time
                                        (float-time idle-time))))
                      (unless (and idle-secs
                                   (> idle-secs ,idle-interval))
                        ,@body))))
     (run-with-idle-timer ,idle-interval t (lambda () ,@body))))

(defmacro silently! (&rest body)
  (declare (indent 0) (debug t))
  `(let ((inhibit-message t)
         (save-silently t))
     (cl-letf (((symbol-function 'message) #'ignore))
       ,@body)))

(defmacro add-transient-hook! (hook-or-function &rest forms)
  "Attaches a self-removing function to HOOK-OR-FUNCTION.

FORMS are evaluated once, when that function/hook is first invoked, then never
again.

HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted function (which will be
advised)."
  (declare (indent 1))
  (let ((append? (if (eq (car forms) :after) (pop forms)))
        (fn (gensym "+transient-hook")))
    `(let ((sym ,hook-or-function))
       (defun ,fn (&rest _)
         ,(format "Transient hook for %S" (+unquote hook-or-function))
         ,@forms
         (let ((sym ,hook-or-function))
           (cond ((functionp sym) (advice-remove sym #',fn))
                 ((symbolp sym)   (remove-hook sym #',fn))))
         (unintern ',fn nil))
       (cond ((functionp sym)
              (advice-add ,hook-or-function ,(if append? :after :before) #',fn))
             ((symbolp sym)
              (put ',fn 'permanent-local-hook t)
              (add-hook sym #',fn ,append?))))))

(defun +chmod-file()
  (interactive)
  (chmod (buffer-file-name) (read-file-modes)))

(defmacro setq-hook! (hooks &rest var-vals)
  "Sets buffer-local variables on HOOKS.
  \(fn HOOKS &rest [SYM VAL]...)"
  (declare (indent 1))
  (macroexp-progn
   (cl-loop for (var val hook fn) in (__setq-hook-fns hooks var-vals)
            collect `(defun ,fn (&rest _)
                       ,(format "%s = %s" var (pp-to-string val))
                       (setq-local ,var ,val))
            collect `(add-hook ',hook #',fn -90))))

(defun __setq-hook-fns (hooks rest &optional singles)
  (unless (or singles (= 0 (% (length rest) 2)))
    (signal 'wrong-number-of-arguments (list #'evenp (length rest))))
  (cl-loop with vars = (let ((args rest)
                             vars)
                         (while args
                           (push (if singles
                                     (list (pop args))
                                   (cons (pop args) (pop args)))
                                 vars))
                         (nreverse vars))
           for hook in (__resolve-hook-forms hooks)
           for mode = (string-remove-suffix "-hook" (symbol-name hook))
           append
           (cl-loop for (var . val) in vars
                    collect
                    (list var val hook
                          (intern (format "__setq-%s-for-%s-h"
                                          var mode))))))

(defun __split-some-window (frame alist)
  "Return a window if splitting any window was successful.
    This function tries using the largest window on FRAME for
    splitting, if all windows are the same size, the selected one is
    taken, in case this fails, the least recently used window is used
    for splitting.  ALIST is passed to `window--try-to-split-window'
    internally."
  (or (window--try-to-split-window (get-largest-window frame t) alist)
      (window--try-to-split-window (get-lru-window frame t) alist)))

(defun __display-buffer-popup-window (buffer alist)
  "Display BUFFER in a popped up window.
    This is a stripped down version of `shackle--display-buffer-popup-window'.
    ALIST is passed to `shackle--window-display-buffer' internally.
    If PLIST contains the :other key with t as value, reuse the next
    available window if possible."
  (let ((window (if (not (one-window-p))
                    (next-window nil 'nominibuf)
                  (__split-some-window (selected-frame) alist))))
    (window--display-buffer buffer window 'window alist)))

"make-dir! check if directory already existis before acting"
(defun make-dir! (dirr)
  (unless (file-exists-p dirr)
      (make-directory dirr)))

(defun try! (fn)
"try execute FN, returning nil if ok, error otherwise."
(condition-case e
    (progn
      (eval fn)
      nil)
  (error e)))

(defun lib/side-window-p ()
  "Return non-nil if the selected window is a side window."
  (window-parameter (selected-window) 'window-side))

(cl-defun lib/file-basename (&optional (file (buffer-file-name)))
  "Return the basename of FILE."
  (file-name-sans-extension (file-name-nondirectory file)))

(defvar escape-hook nil
  "A hook run when `C-g' is pressed (or ESC in normal mode, for evil users).")

(defun escape (&optional interactive)
  "Run `escape-hook'.
INTERACTIVE means that accept the Universal Argument `C-u'"
  (interactive (list 'interactive))
  (cond ((minibuffer-window-active-p (minibuffer-window))
         ;; quit the minibuffer if open.
         (when interactive
           (setq this-command 'abort-recursive-edit))
         (abort-recursive-edit))
        ;; Run all escape hooks. If any returns non-nil, then stop there.
        ((run-hook-with-args-until-success 'escape-hook))
        ;; don't abort macros
        ((or defining-kbd-macro executing-kbd-macro) nil)
        ;; Back to the default
        ((unwind-protect (keyboard-quit)
           (when interactive
             (setq this-command 'keyboard-quit))))))


(defconst interactive-modes-alist
  '(emacs-lisp-mode
    lisp-interaction-mode
    eshell-mode
    clojure-mode
    lisp-mode
    rustic-mode
    rust-mode
    python-mode
    js-mode
    ts-mode
    go-mode
    haskell-mode
    org-mode
    eshell-mode
    shell-script-mode
    prog-mode
    text-mode)
  "List containg all modes that user can write with")

(defconst my/global-map
  '(global-map
    org-mode-map
    prog-mode-map
    text-mode-map
    emacs-lisp-mode-map
    diff-mode-map
    help-mode-map
    eww-mode-map
    pdf-view-mode-map
    lisp-mode-map
    gnus-mode-map
    vterm-mode-map
    shell-mode-map
    eshell-mode-map
    dirvish-mode-map
    message-mode-map
    ibuffer-mode-map
    magit-mode-map
    magit-log-mode-map
    comint-mode-map)
  "List of main modes for keymaps.")

(defmacro after-frame! (&rest body)
  "Run BODY after each frame is created, useful for interface customization."
  `(add-hook 'after-make-frame-functions
          (lambda (frame)
            (select-frame frame)
            (lambda () ,body))))

(defconst ju//minibuffer-maps
  '(minibuffer-local-map
    minibuffer-local-ns-map
    minibuffer-local-completion-map
    minibuffer-local-must-match-map
    minibuffer-local-isearch-map)
  "List of minibuffer keymaps.")

(with-eval-after-load 'eldoc
  (eldoc-add-command 'escape))

(defalias 'expand #'expand-file-name)
(defalias 'bol #'line-beginning-position)
(defalias 'eol #'line-end-position)

(defun buffer-unique-p ()
  "Check if the current buffer is unique among all windows."
  (let ((buffer (current-buffer)))
    (not (cl-loop for win being the windows
                  if (eq buffer (window-buffer win))
                  collect win))))

(defun side-window-p ()
  "Return non-nil if the selected window is a side window."
  (window-parameter (selected-window) 'window-side))

(defmacro run-at-active-interval (interval idle-interval &rest body)
  "Every INTERVAL seconds, unless idle for > IDLE-INTERVAL seconds, run BODY.
Also, after IDLE-INTERVAL seconds of idle time, run BODY. This allows using an
idle timer to quickly run BODY when Emacs becomes idle but also ensures that
BODY is run periodically even if Emacs is actively being used."
  (declare (indent 2))
  `(progn
     (run-at-time (current-time) ,interval
                  (lambda ()
                    (let* ((idle-time (current-idle-time))
                           (idle-secs (when idle-time
                                        (float-time idle-time))))
                      (unless (and idle-secs
                                   (> idle-secs ,idle-interval))
                        ,@body))))
     (run-with-idle-timer ,idle-interval t (lambda () ,@body))))

;;;###autoload
(defun kill-this-buffer+ ()
  "`kill-this-buffer' with no menu-bar checks.
`kill-this-buffer' is supposed to be called from the menu bar.
See https://www.reddit.com/r/emacs/comments/64xb3q/killthisbuffer_sometimes_just_stops_working/."
  (interactive)
  (if (minibufferp)
      (abort-recursive-edit)
    (kill-buffer (current-buffer))))

(defvar +lisp-modes
  '(emacs-lisp-mode
    lisp-interaction-mode
    eshell-mode
    clojure-mode
    lisp-mode)
  "List for all used lisp modes.")

;;;###autoload
(defun close-minibuffer ()
  "Close the current buffer if its major mode is in the `modes` list."
  (interactive)
  (unless (memq major-mode interactive-modes-alist)
      (progn
        (kill-buffer (current-buffer))
        (unless (one-window-p)
          (delete-window)))
    (message (format "Closing %s ..." major-mode))))

;;;###autoload
(defun kill-buffer-delete-window ()
  "Kill the current buffer and then delete the current window."
  (interactive)
  (if (one-window-p)
      (kill-this-buffer+)
    (progn
      (when (buffer-unique-p)
        (kill-this-buffer+))
      (delete-window))))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;;;###autoload
(defun manual-save-buffer ()
  "Save buffer and remove trailing whitespace.
this method is necessary (instead of overwriting the default) to avoid messing
with auto-save behavior."
  (interactive)
  (delete-trailing-whitespace)
  (save-buffer))

;;;###autoload
(defun really-kill-current-buffer ()
  "Kill the current buffer - even if modified.
Useful to use on stuck buffers opened with `emacsclient'"
  (interactive)
  (set-buffer-modified-p nil)
  (read-only-mode -1)
  (let ((buf-name (buffer-name)))
    (when (string-match "/" buf-name)
        (setq buf-name (replace-regexp-in-string "/" "-" buf-name)))
              ;; setq buf-name (replace "/" with "-")
  (write-file (make-temp-file (concat "/tmp/emacs/" buf-name))))
  (kill-buffer (current-buffer)))

(defun insert-or-update-header-footer ()
  "Insert or update the header and footer of the current file."
  (interactive)
  (let* ((file-name (file-name-nondirectory (buffer-file-name)))
         (header (concat (format ";;; %s " file-name)
                         "---  desc  -*- lexical-binding: t; -*-\n"
                         ";;; Commentary:\n;;; Code:\n"))
         (footer (format ";;; %s ends here" file-name)))
    (save-excursion
      (goto-char (point-min))
      (if (re-search-forward (regexp-quote header) nil t)
          (replace-match header)
        (insert header "\n"))
      (goto-char (point-max))
      (if (re-search-backward (regexp-quote footer) nil t)
          (replace-match footer)
        (insert "\n" footer)))
    (message "Header and footer updated in %s" file-name)))

(defun ju-hash-to-list (HashTable)
  (let ((xx nil))
    (maphash
     (lambda (k v)
       (push (list  k v) xx))
     HashTable)
    xx))

(defmacro cmds! (&rest branches)
  "Returns a dispatcher that runs the a command in BRANCHES.
Meant to be used as a target for keybinds (e.g. with `define-key' or `general-def').

BRANCHES is a flat list of CONDITION COMMAND pairs. CONDITION is a lisp form
that is evaluated when (and each time) the dispatcher is invoked. If it returns
non-nil, COMMAND is invoked, otherwise it falls through to the next pair.

The last element of BRANCHES can be a COMMANd with no CONDITION. This acts as
the fallback if all other conditions fail.

Otherwise, Emacs will fall through the keybind and search the next keymap for a
keybind (as if this keybind never existed).

See `general-key-dispatch' for what other arguments it accepts in BRANCHES."
  (declare (doc-string 1))
  (let ((docstring (if (stringp (car branches)) (pop branches) ""))
        fallback)
    (when (cl-oddp (length branches))
      (setq fallback (car (last branches))
            branches (butlast branches)))
    (let ((defs (cl-loop for (key value) on branches by 'cddr
                         unless (keywordp key)
                         collect (list key value))))
      `'(menu-item
         ,(or docstring "") nil
         :filter (lambda (&optional _)
                   (let (it)
                     (cond ,@(mapcar (lambda (pred-def)
                                       `((setq it ,(car pred-def))
                                         ,(cadr pred-def)))
                                     defs)
                           (t ,fallback))))))))

(defmacro first-arg! (arg)
  `(if (string-match ,`(format "\\b\\(%s\\)\\b" ,arg) ,arg)
       (match-string 1 ,arg)))

(defmacro first-word! (arg)
  `(if (string-match ,`(format "\\b\\(%s\\).*" ,arg) ,arg)
       (match-string 1 ,arg)))

(defmacro symbol-or-self! (val)
  `(if ,(ignore-error (symbol-name val))
       (symbol-name ,val)
     ,val))

;;;###autoload
(defvar ju--counter 0
  "counter to use to prevent name clashes for automatically named functions.")

;;;###autoload
(defmacro ju--ensure-lists (&rest vars)
  "Ensure that all variables in VARS are lists if they are not already.
If any variable is a lambda, it will not be considered to be a list. If a var is
nil, it will be set to (list nil)."
  `(progn
     ,@(mapcar (lambda (var)
                 `(unless (and ,var
                               (listp ,var)
                               ;; lambdas are lists
                               (not (functionp ,var)))
                    (setq ,var (list ,var))))
               vars)))

;;;###autoload
(defun ju--define-transient-function (function hook &optional advice
                                                    condition)
  "Define and return a modified FUNCTION that removes itself from HOOK.
The new function will automatically remove itself from HOOK after the first time
it is called. If ADVICE is non-nil, HOOK should specify a function to advise
instead. If CONDITION is a function, only remove the function if calling
CONDITION on the return value returns true. For example, if CONDITION is
#'identity, only remove the function if it returns non-nil."
  (let ((name (intern (format "general--transient-%s%s%s"
                              (if (symbolp function)
                                  (symbol-name function)
                                ;; lambda; name with counter
                                (cl-incf ju--counter))
                              (if advice
                                  "-for-advice"
                                "-for-hook")
                              (if (functionp condition)
                                  (if (symbolp function)
                                      (format "-on-%s" condition)
                                    ;; lambda; name with counter
                                    (format "-on-lambda-%s"
                                            (cl-incf ju--counter)))
                                "")))))
    (defalias name
      (if advice
          (lambda (&rest args)
            (let ((res (apply function args)))
              (when (or (not (functionp condition)) (funcall condition res))
                (advice-remove hook name)
                (fmakunbound name))
              res))
        (lambda (&rest args)
          (let ((res (apply function args)))
            (when (or (not (functionp condition)) (funcall condition res))
              (remove-hook hook name)
              (fmakunbound name))
            res)))
      (format "Call %s with ARGS and then remove it from `%s'%s."
              (if (symbolp function)
                  (format "`%s'" function)
                ;; TODO put full lambda in docstring or use backquote instead of
                ;; relying on lexical-binding (so full lambda is in definition)
                "given lambda")
              hook
              (if (functionp condition)
                  (format " once calling %s on the return value succeeds."
                          (if (symbolp condition)
                              condition
                            "given lambda"))
                "")))
    name))

;;;###autoload
(defun ju-add-hook (hooks functions &optional append local transient)
  "A drop-in replacement for `add-hook'.
Unlike `add-hook', HOOKS and FUNCTIONS can be single items or lists. APPEND and
LOCAL are passed directly to `add-hook'. When TRANSIENT is non-nil, each
function will remove itself from the hook it is in after it is run once. If
TRANSIENT is a function, call it on the return value in order to determine
whether to remove a function from the hook. For example, if TRANSIENT is
#'identity, remove each function only if it returns non-nil. TRANSIENT could
alternatively check something external and ignore the function's return value."
  (ju--ensure-lists hooks functions)
  (dolist (hook hooks)
    (dolist (func functions)
      (when transient
        (setq func (ju--define-transient-function
                    func hook nil transient)))
      (add-hook hook func append local))))

;;;###autoload
(defun ju-remove-hook (hooks functions &optional local)
  "A drop-in replacement for `remove-hook'.
Unlike `remove-hook', HOOKS and FUNCTIONS can be single items or lists. LOCAL is
passed directly to `remove-hook'."
  (ju--ensure-lists hooks functions)
  (dolist (hook hooks)
    (dolist (func functions)
      (remove-hook hook func local))))

;;;###autoload
(defmacro ju-after-gui (&rest body)
  "Run BODY once after the first GUI frame is created."
  (declare (indent 0) (debug t))
  `(if (and (not (daemonp)) (display-graphic-p))
       (progn ,@body)
     (ju-add-hook 'server-after-make-frame-hook
                       (lambda ()
                         (when (display-graphic-p)
                           ,@body
                           t))
                       nil
                       nil
                       #'identity)))

(defmacro ju-after-tty (&rest body)
  "Run BODY once after the first terminal frame is created."
  (declare (indent 0) (debug t))
  `(if (and (not (daemonp)) (not (display-graphic-p)))
       (progn ,@body)
     (ju-add-hook 'server-after-make-frame-hook
                       (lambda ()
                         (unless (display-graphic-p)
                           ,@body
                           t))
                       nil
                       nil
                       #'identity)))

;;;###autoload
(defmacro ju-after-init (&rest body)
  "Run BODY after emacs initialization.
If after emacs initialization already, run BODY now."
  (declare (indent 0) (debug t))
  `(if after-init-time
       (progn ,@body)
     (ju-add-hook 'after-init-hook (lambda () ,@body))))

(provide 'core-lib)
;;; core-lib.el ends here
