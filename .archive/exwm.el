;;;; init-exwm.el                                 -*- lexical-binding: t; -*-

(require 'exwm)
;; (require 'exwm-config)

;; Commented out because I'm using stalonetray instead of the EXWM tray.
;; (require 'exwm-systemtray)
;; (exwm-systemtray-enable)
;; (exwm-config-default)

(require 'exwm-randr)

;;;; Workspaces.
(setq exwm-workspace-number 10)
(setq exwm-workspace-show-all-buffers t)

;; Glitch fix: when switching to a window not in the current
;; workspace, move it to the workspace first.
(defadvice set-window-buffer (after elmord-exwm-auto-move-workspace)
  (let* ((window (or (ad-get-arg 0) (selected-window)))
         (frame (window-frame window))
         (buffer (ad-get-arg 1))
         (exwm-id (exwm--buffer->id buffer)))
    (when (and exwm-id (exwm-workspace--workspace-p frame))
      (set-frame-selected-window frame window)
      (exwm-workspace-move-window frame exwm-id))))

(ad-activate 'set-window-buffer)

;; 's-&': Launch application
(exwm-input-set-key (kbd "s-&")
  (defun elmord-exwm-shell-command (command)
    (interactive (list (read-shell-command "$ ")))
    (let ((default-directory "~"))
      (start-process "exwm-subproc" nil
                     "bash" "-c" "nohup bash -c \"$1\" >/dev/null 2>&1" "--" command))))

;; Update buffer name based on class and title.
(defvar elmord-exwm-buffer-name-limit 40)

(defun elmord-exwm-terminal-p (&optional class-name)
  (member (or class-name exwm-class-name)
          '("Xfce4-terminal" "X-terminal-emulator" "Lxterminal")))

(defun elmord-exwm-compute-buffer-name ()
  (let ((class (or exwm-class-name ""))
        (title (or exwm-title "")))
    (cond
     ((and (member class '("Firefox" "Firefox-esr" "Icedove"))
           (string-match "\\`\\(.*\\)\\( - [^-]*\\)\\'" title))
      (concat class ": " (match-string 1 title)))
     ((member class '("Telegram" "TelegramDesktop"))
      (if (equal title "") "Telegram" title))
     ((elmord-exwm-terminal-p class)
      (concat "Term" ": " title))
     ((equal class "skypeforlinux") "Skype")
     (t (concat class ": " title)))))

(defun elmord-exwm-update-buffer-name ()
  (exwm-workspace-rename-buffer
   (truncate-string-to-width
    (elmord-exwm-compute-buffer-name)
    elmord-exwm-buffer-name-limit
    nil  ; start
    nil  ; padding
    "…")
    ))

(add-hook 'exwm-update-class-hook 'elmord-exwm-update-buffer-name)
(add-hook 'exwm-update-title-hook 'elmord-exwm-update-buffer-name)

;; Pass on C-u intact to applications.
(setq exwm-input-prefix-keys
      (delq ?\C-u exwm-input-prefix-keys))

;;;; Global bindings.
(exwm-input-set-key (kbd "s-r") 'rename-buffer)
(exwm-input-set-key (kbd "s-R") 'exwm-reset)
(exwm-input-set-key (kbd "s-w") 'exwm-workspace-switch)
(exwm-input-set-key (kbd "s-o") 'other-window)
(exwm-input-set-key (kbd "s-m") 'exwm-workspace-move-window)
(exwm-input-set-key (kbd "s-q") 'delete-window)
(exwm-input-set-key (kbd "s-k") 'kill-buffer)

;; Switch workspace with Super+number
(dotimes (i 10)
  (exwm-input-set-key (kbd (format "s-%d" i))
                      `(lambda ()
                         (interactive)
                         (exwm-workspace-switch-create ,i))))

;;;; EXWM line mode bindings.
;; C-x C-x to send C-x, C-c C-c to send C-c.
(define-key exwm-mode-map (kbd "C-x C-x")
  (lambda () (interactive) (exwm-input--fake-key ?\C-x)))
(define-key exwm-mode-map (kbd "C-c C-c")
  (lambda () (interactive) (exwm-input--fake-key ?\C-c)))

;; Line-editing shortcuts
;; (exwm-input-set-simulation-keys
;;  '(([?\C-b] . left)
;;    ([?\C-f] . right)
;;    ([?\C-p] . up)
;;    ([?\C-n] . down)
;;    ([?\C-a] . home)
;;    ([?\C-e] . end)
;;    ([?\M-v] . prior)
;;    ([?\C-v] . next)
;;    ([?\C-d] . delete)
;;    ([?\C-k] . (S-end delete))))

(defun elmord-exwm-next-workspace (&optional incr)
  (interactive "p")
  (when (not incr) (setq incr 1))
  (let* ((count (exwm-workspace--count))
         (index (mod (+ exwm-workspace-current-index incr) count)))
    (exwm-workspace-switch index)))

(defun elmord-exwm-previous-workspace (&optional incr)
  (interactive "p")
  (when (not incr) (setq incr 1))
  (elmord-exwm-next-workspace (- incr)))

(exwm-input-set-key (kbd "s-<left>") 'elmord-exwm-previous-workspace)
(exwm-input-set-key (kbd "s-<right>") 'elmord-exwm-next-workspace)

(defun elmord-exwm-buffer-list (&optional predicate)
  (cl-remove-if-not
   (lambda (buffer)
     (and (eq (buffer-local-value 'major-mode buffer) 'exwm-mode)
          (if predicate (funcall predicate buffer) t)))
   (buffer-list)))

(defun elmord-ido-read-exwm-buffer (prompt)
  "Read an EXWM buffer name from the user."
  (let ((buffers (elmord-exwm-buffer-list)))
    (ido-completing-read prompt (mapcar 'buffer-name buffers))))

(defun elmord-ido-read-buffer-maybe-exwm (pre-prompt only-exwm)
  "Read a buffer name from the user. if ONLY-EXWM is non-nil,
only present EXWM buffers as options."
  (let ((prompt (format "%s%s: " pre-prompt (if only-exwm " (EXWM)" "")))
        (prompter (if only-exwm 'elmord-ido-read-exwm-buffer 'ido-read-buffer)))
    (funcall prompter prompt)))

(defun elmord-exwm-ido-jump-to-buffer (only-exwm)
  "Jump to the workspace containing an EXWM buffer."
  (interactive "P")
  (let ((bufname (elmord-ido-read-buffer-maybe-exwm "Jump to buffer" only-exwm)))
    (exwm-workspace-switch-to-buffer bufname)))

(defun elmord-exwm-ido-bring-buffer (only-exwm)
  "Bring an EXWM buffer to the current workspace."
  (interactive "P")
  (let* ((bufname (elmord-ido-read-buffer-maybe-exwm "Bring buffer" only-exwm)))
    (switch-to-buffer bufname)))

(defun elmord-exwm-ido-jump-to-exwm-buffer ()
  (interactive)
  (elmord-exwm-ido-jump-to-buffer t))

(defun elmord-exwm-ido-bring-exwm-buffer ()
  (interactive)
  (elmord-exwm-ido-bring-buffer t))

(exwm-input-set-key (kbd "s-j") 'elmord-exwm-ido-jump-to-buffer)
(exwm-input-set-key (kbd "s-J") 'elmord-exwm-ido-jump-to-exwm-buffer)
(exwm-input-set-key (kbd "s-b") 'elmord-exwm-ido-bring-buffer)
(exwm-input-set-key (kbd "s-B") 'elmord-exwm-ido-bring-exwm-buffer)

(display-time-mode 1)
(display-battery-mode 1)

;; Shortcuts.
(defun elmord-exwm-shortcut (key command)
  (exwm-input-set-key key
    `(lambda ()
       (interactive)
       (start-process "exwm-shortcut" nil "sh" "-c" ,command))))

(elmord-exwm-shortcut (kbd "s--") "amixer set Master 3%-")
(elmord-exwm-shortcut (kbd "s-=") "amixer set Master 3%+")
(elmord-exwm-shortcut (kbd "s-\\") "musicctl play")
(elmord-exwm-shortcut (kbd "s-(") "musicctl prev")
(elmord-exwm-shortcut (kbd "s-)") "musicctl next")

(elmord-exwm-shortcut (kbd "<XF86AudioLowerVolume>") "amixer set Master 3%-")
(elmord-exwm-shortcut (kbd "<XF86AudioRaiseVolume>") "amixer set Master 3%+")
(elmord-exwm-shortcut (kbd "<XF86AudioPlay>") "musicctl play")

(elmord-exwm-shortcut (kbd "<XF86PowerOff>") "sudo systemctl suspend")

(elmord-exwm-shortcut (kbd "s-x") "sleep 0.2; xset dpms force suspend")
(elmord-exwm-shortcut (kbd "<XF86MonBrightnessUp>") "backlight-brightness +")
(elmord-exwm-shortcut (kbd "<XF86MonBrightnessDown>") "backlight-brightness -")

;;(elmord-exwm-shortcut (kbd "s-t") "x-terminal-emulator")
(elmord-exwm-shortcut (kbd "s-L") "gnome-screensaver-command -l")

(defun elmord-exwm-switch-or-open-other-window (selector command)
  (let ((candidates (cl-remove-if-not
                     (lambda (buffer)
                              (with-current-buffer buffer
                                (and (equal major-mode 'exwm-mode)
                                     (funcall selector))))
                     (buffer-list))))
    (if candidates
        (progn
          (switch-to-buffer-other-window (car candidates))
          (exwm-workspace-move-window
           exwm-workspace-current-index
           (buffer-local-value 'exwm--id (car candidates)))
          )
      (switch-to-buffer-other-window "*scratch*")
      (start-process "exwm-shortcut" nil "sh" "-c" command))))


(exwm-input-set-key (kbd "s-t")
  (lambda ()
    (interactive)
    (elmord-exwm-switch-or-open-other-window
     'elmord-exwm-terminal-p "x-terminal-emulator")))

(exwm-input-set-key (kbd "s-v")
  (lambda ()
    (interactive)
    (elmord-exwm-switch-or-open-other-window
     (lambda () (equal exwm-class-name "vlc"))
     "vlc")))

(define-prefix-command 'super-g-map)

(exwm-input-set-key (kbd "s-g") 'super-g-map)

(defvar *elmord-exwm-super-g-commands*
  '((:key "f" :class "Firefox" :command "firefox")))

(dolist (command *elmord-exwm-super-g-commands*)
  (define-key super-g-map (kbd (plist-get command :key))
    `(lambda ()
       (interactive)
       (elmord-exwm-switch-or-open-other-window
        (lambda () (equal exwm-class-name ,(plist-get command :class)))
        ,(plist-get command :command)))))



;;;; Urgency handling.
(defun elmord-exwm-buffer-urgent-p (buffer)
  (or
   (buffer-local-value 'exwm--hints-urgency buffer)
   ;;(elmord-exwm-telegram-urgent-p buffer)
   ;;(elmord-exwm-evolution-urgent-p buffer)
   ;;(elmord-exwm-chromium-skype-urgent-p buffer)
   ))

(defun elmord-exwm-urgent-buffer-list ()
  (elmord-exwm-buffer-list 'elmord-exwm-buffer-urgent-p))

(defun elmord-exwm-jump-to-urgent-buffer ()
  "Jump to the first urgent window."
  (interactive)
  (let ((urgent-buffers (elmord-exwm-urgent-buffer-list)))
    (if (not urgent-buffers)
        (message "No urgent buffers")
      (let ((buffer (car urgent-buffers)))
        (exwm-workspace-switch-to-buffer buffer)
        (elmord-update-mode-line-exwm-urgency)))))

;; When visiting a window, remove its urgency.
(add-hook 'buffer-list-update-hook
  (defun elmord-exwm-remove-urgency ()
    (when (and (eq major-mode 'exwm-mode)
               (eq (current-buffer) (window-buffer))
               exwm--hints-urgency)
      (setf exwm--hints-urgency nil)
      (run-hooks 'elmord-exwm-update-hints-hook))))

(exwm-input-set-key (kbd "s-u") 'elmord-exwm-jump-to-urgent-buffer)

;; A hook for tracking when windows become urgent.
(defvar elmord-exwm-update-hints-hook nil
  "Normal hook run when window hints are updated.")

(defadvice exwm--update-hints (after elmord-update-hints-hook activate)
  (run-hooks 'elmord-exwm-update-hints-hook))

;; Indicate urgent windows in the mode line.
(defvar elmord-mode-line-exwm-urgency "")
(put 'elmord-mode-line-exwm-urgency 'risky-local-variable t)
(setq mode-line-misc-info
      (append mode-line-misc-info (list 'elmord-mode-line-exwm-urgency)))

(defun elmord-update-mode-line-exwm-urgency ()
  (let ((text (string-join
               (mapcar 'buffer-name (elmord-exwm-urgent-buffer-list))
               ", ")))
    (setq elmord-mode-line-exwm-urgency
          `(:propertize ,text
                        face
                        (:foreground "white"
                         :background "#0000a0")))
    (force-mode-line-update t)))

(add-hook 'elmord-exwm-update-hints-hook 'elmord-update-mode-line-exwm-urgency)
(add-hook 'exwm-update-title-hook 'elmord-update-mode-line-exwm-urgency)


;;;;;;;;;;;;;;

(setq exwm-manage-configurations
      '(((equal exwm-class-name "Firefox") workspace 0)
        ((equal exwm-class-name "Firefox-esr") workspace 0)
        ((equal exwm-class-name "skypeforlinux") workspace 9)
        ((equal exwm-class-name "Slack") workspace 9)
        ((equal exwm-class-name "Chromium-browser") workspace 9)
        ((equal exwm-class-name "Chromium") workspace 9)
        ((equal exwm-class-name "Evolution") workspace 8)
        ((equal exwm-class-name "Icedove") workspace 8)
        ((equal exwm-class-name "Thunderbird") workspace 8)
        ((equal exwm-class-name "Telegram") workspace 7)
        ((equal exwm-class-name "TelegramDesktop") workspace 7)
        ((equal exwm-class-name "Pidgin") workspace 6)
        ))


;; Make C-x 8 RET work with any program supporting paste from clipboard with C-v.

(defun elmord-exwm-get-paste-key ()
  (cond ((elmord-exwm-terminal-p)  (aref (kbd "S-C-v") 0))
        (t                         ?\C-v)))

(defun elmord-exwm-send-string (string)
  (interactive "MSend string: ")
  (let ((saved-clipboard (gui-backend-get-selection 'CLIPBOARD 'STRING))
        (paste-key (elmord-exwm-get-paste-key)))
    (gui-backend-set-selection 'CLIPBOARD string)
    (run-at-time 0.1 nil
      `(lambda ()
         (exwm-input--fake-key ,paste-key)
         (run-at-time 0.3 nil
           `(lambda ()
              (gui-backend-set-selection 'CLIPBOARD ,,saved-clipboard))))))
  t)

(defun elmord-exwm-send-character (char)
  (interactive
   ;; Copied from `insert-char' source.
   (list (read-char-by-name "Insert character for EXWM (Unicode name or hex): ")))
  (elmord-exwm-send-string (string char)))

(define-key exwm-mode-map (kbd "C-x 8 RET") 'elmord-exwm-send-character)

(defun elmord-exwm-send-keys (string)
  (interactive "MSend keys: ")
  (elmord-exwm--send-key-list (string-to-list string)))

(defun elmord-exwm--send-key-list (list)
  (when list
    (run-at-time 0.1 nil
      (lambda ()
        (exwm-input--fake-key (car list))
        (elmord-exwm--send-key-list (cdr list))))))

(defun elmord-remove-from-tree (item tree)
  (cond
   ((and (consp tree)
         (eq (car tree) item))
    (elmord-remove-from-tree item (cdr tree)))
   ((consp tree)
    (cons (elmord-remove-from-tree item (car tree))
     (elmord-remove-from-tree item (cdr tree))))
   (t tree)))


(defun elmord-exwm-kill-buffer-hook ()
  (let ((window (get-buffer-window (current-buffer) 'visible)))
    (when window
      (condition-case err
          (delete-window window)
        (error (unless (equal (cdr err)
                              '("Attempt to delete minibuffer or sole ordinary window"))
                 (signal (car err) (cdr err))))))))

(defun elmord-exwm-terminal-cd ()
  (when (string-match "\\`<\\([^>]*\\)>" exwm-title)
    (cd (match-string 1 exwm-title))))

(add-hook 'exwm-manage-finish-hook
  (defun elmord-exwm-manage-finish-hook ()
    ;; We don't need line position in an EXWM buffer.
    (setq-local mode-line-format
                (elmord-remove-from-tree 'mode-line-position mode-line-format))
    (add-hook 'kill-buffer-hook 'elmord-exwm-kill-buffer-hook nil t)
    (when (elmord-exwm-terminal-p)
      (add-hook 'exwm-update-title-hook 'elmord-exwm-terminal-cd))))


;;;; Start pidgin in background

(fset '~exwm-floating--unset-floating (symbol-function 'exwm-floating--unset-floating))
(fset '~exwm-layout--iconic-state-p (symbol-function 'exwm-layout--iconic-state-p))

;; This is a terrible kludge to make EXWM think the window is
;; iconified when it is created.
(defadvice exwm-manage--manage-window (around elmord-start-in-background activate)
  (cl-letf (((symbol-function 'exwm-floating--unset-floating)
             (lambda (id)
               (cl-letf (((symbol-function 'exwm-layout--iconic-state-p)
                          (lambda (&optional id)
                            (or (~exwm-layout--iconic-state-p id)
                                (elmord-exwm-start-in-background-p id)))))
                 (~exwm-floating--unset-floating id)))))
    ad-do-it
    (exwm-layout--refresh)))

(defun elmord-exwm-start-in-background-p (id)
  ;; TODO: Do whatever other window managers do to figure out that a
  ;; window should be started in background, rather than tying it to Pidgin.
  (with-current-buffer (if id (exwm--id->buffer id) (current-buffer))
    (equal exwm-class-name "Pidgin")))


;; Switching buffers in a fixed sequence.
(defun elmord-sorted-buffers (&optional reverse-p)
  (sort (buffer-list)
        (if reverse-p
            (lambda (a b) (string> (buffer-name a) (buffer-name b)))
          (lambda (a b) (string< (buffer-name a) (buffer-name b))))))

(defun elmord-next-buffer (&optional reverse-p)
  (let* ((buffers (elmord-sorted-buffers reverse-p))
         (next-buffers (member (current-buffer) buffers)))
    (cond ((null buffers) (error "Can't happen!"))
          ((null (cdr next-buffers)) (car buffers))
          (t (cadr next-buffers)))))

(defun elmord-switch-to-next-buffer ()
  (interactive)
  (message "%S" (elmord-next-buffer))
  (switch-to-buffer (elmord-next-buffer)))

(defun elmord-switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (elmord-next-buffer t)))


(defun elmord-list-all-buffers-with-class (class &optional reverse-p)
  (let ((cmp (if reverse-p #'string> #'string<)))
    (if class
        (elmord-list-exwm-buffers-with-class class cmp)
      (elmord-list-regular-buffers cmp))))

(defun elmord-list-exwm-buffers-with-class (class cmp)
  (sort (elmord-exwm-buffer-list
         (lambda (buffer)
           (equal (buffer-local-value 'exwm-class-name buffer) class)))
        (lambda (a b)
          (funcall cmp (buffer-name a) (buffer-name b)))))

(defun elmord-list-regular-buffers (cmp)
  (let ((buffers (cl-remove-if (lambda (buffer)
                                 (eq (buffer-local-value 'major-mode buffer) 'exwm-mode)))))
    (sort buffers (lambda (a b)
                    (funcall cmp
                             (or (buffer-file-name a) (buffer-name a))
                             (or (buffer-file-name b) (buffer-name b)))))))


(defun elmord-exwm-get-next-buffer-same-class (&optional reverse-p)
  (let* ((buffers (elmord-list-all-buffers-with-class
                   exwm-class-name
                   reverse-p))
         (next-buffers (member (current-buffer) buffers)))
    (cond ((null next-buffers) (error "Not an EXWM buffer"))
          ((null (cdr buffers)) (error "No other `%s' buffers" exwm-class-name))
          ((null (cdr next-buffers)) (car buffers))
          (t (cadr next-buffers)))))

(defun elmord-exwm-next-buffer-same-class ()
  (interactive)
  (switch-to-buffer (elmord-exwm-get-next-buffer-same-class)))

(defun elmord-exwm-previous-buffer-same-class ()
  (interactive)
  (switch-to-buffer (elmord-exwm-get-next-buffer-same-class t)))


(exwm-input-set-key (kbd "s-<up>") 'elmord-exwm-previous-buffer-same-class)
(exwm-input-set-key (kbd "s-<down>") 'elmord-exwm-next-buffer-same-class)

;;;;;;; experimental stuff ;;;;;;;;;;


(defun elmord-exwm-window-pid (&optional id)
  (when (not id)
    (setq id exwm--id))
  (when (not id)
    (error "Window id not provided and could not be determined"))
  (let ((reply (xcb:+request-unchecked+reply exwm--connection
                   (make-instance 'xcb:ewmh:get-_NET_WM_PID :window id))))
    (slot-value reply 'value)))

(defun elmord-exwm-window-command-info (&optional id)
  (let* ((pid (elmord-exwm-window-pid id))
         (cmdline (with-temp-buffer
                    (insert-file-contents (format "/proc/%d/cmdline" pid))
                    (buffer-string))))
    `((pid . ,pid)
      (command-line
       . ,(split-string (string-remove-suffix "\0" cmdline) "\0"))
      (working-directory
       . ,(file-chase-links (format "/proc/%d/cwd" pid))))))

(defun elmord-exwm-load-evince-info ()
  (let* ((info (elmord-exwm-window-command-info))
         (pid (cdr (assoc 'pid info)))
         (basename (car (last (cdr (assoc 'command-line info)))))
         (fullname (elmord-find-matching-symlink
                    basename
                    (format "/proc/%d/fd" pid))))
    (setq elmord-evince-file-name fullname)
    (setq elmord-evince-url
          (elmord-read-command-output
           "get_firefox_download_url.py" fullname))
    (setq elmord-evince-title
          (elmord-read-command-output "pdftitle" fullname))
    (setq elmord-evince-sha1
          (car (split-string (elmord-read-command-output "sha1sum" fullname))))))


(defun elmord-read-command-output (command &rest args)
    (string-remove-suffix
     "\n"
     (with-temp-buffer
       (apply #'call-process command nil t nil args)
       (buffer-string))))

(defun elmord-find-matching-symlink (file dir)
  (cl-block nil
    (let ((basename (concat "/" (file-name-nondirectory file)))
          (entries (mapcar #'cl-second (directory-files-and-attributes dir t))))
      (dolist (entry entries)
        (when (and (stringp entry)
                   (string-suffix-p basename entry))
          (cl-return entry))))))

(defun elmord-exwm-evince-log ()
  (elmord-exwm-load-evince-info)
  (with-current-buffer (find-file-noselect "~/org/papers.org")
    (let* ((id (concat "sha1-" elmord-evince-sha1))
           (entry (org-find-entry-with-id id)))
      (when (not entry)
        (goto-char (point-max))
        (insert "\n* " elmord-evince-title "\n")
        (org-entry-put (point) "ID" id)
        (org-entry-put (point) "URL" elmord-evince-url)
        (org-entry-put (point) "DATE" (with-temp-buffer
                                        (org-insert-time-stamp (current-time) t t)))
        (save-buffer)))))

;;(add-hook 'exwm-init-hook
;;  (defun elmord-exwm-evince-hook ()
;;    (when (equal exwm-class-name "Evince")
;;      (elmord-exwm-evince-log)
;;      (local-set-key [remap save-buffer] 'elmord-exwm-evince-save)
;;      )))
;;
(defun elmord-exwm-evince-save ()
  (interactive)
  (let ((filename (read-file-name
                   "Save PDF: "
                   "/home/xyz/Papers/"
                   nil
                   nil
                   (concat elmord-evince-title ".pdf"))))
    (copy-file elmord-evince-file-name filename)
    (with-current-buffer (find-file-noselect "~/org/papers.org")
      (let ((entry (org-find-entry-with-id (concat "sha1-" elmord-evince-sha1))))
        (when entry
          (org-entry-put entry "FILENAME" filename))))))

;;;;;;;;; org-capture with EXWM buffers.

(defun elmord-exwm-get-firefox-url ()
  (exwm-input--fake-key ?\C-l)
  (sleep-for 0.05)
  (exwm-input--fake-key ?\C-c)
  (sleep-for 0.05)
  (gui-backend-get-selection 'CLIPBOARD 'STRING))

(defun elmord-exwm-org-store-link ()
  (when (and (equal major-mode 'exwm-mode)
             (member exwm-class-name '("Firefox" "Firefox-esr")))
    (org-store-link-props
     :type "http"
     :link (elmord-exwm-get-firefox-url)
     :description exwm-title)))

;;(add-to-list 'org-store-link-functions 'elmord-exwm-org-store-link)


;;;;;;;;

(defun elmord-exwm-send-delayed-keys (keys delay)
  (dolist (key keys)
    (exwm-input--fake-key key)
    (sleep-for delay)))


(defun elmord-exwm-get-firefox-content ()
  ;;(elmord-exwm-send-delayed-keys '(?\C-f escape ?\C-a ?\C-c) 0.05)
  (let* ((content (decode-coding-string (gui-backend-get-selection 'PRIMARY 'text/html) 'utf-16le))
         (url (decode-coding-string (gui-backend-get-selection 'PRIMARY 'text/x-moz-url-priv) 'utf-16le)))
    (elmord-exwm-send-delayed-keys '(?\C-f delete escape) 0.05) ;; unselect text, meh
    (list :content content :url url :title exwm-title)))

(defun elmord-exwm-save-firefox-content ()
  (let ((data (elmord-exwm-get-firefox-content)))
    (with-current-buffer (find-file-noselect "~/org/browser.org")
      (insert "\n* " (cl-getf data :title) "\n")
      (let ((pandoc (start-process-shell-command
                      "pandoc"
                      (current-buffer)
                      "pandoc -f html -t haddock | pandoc -f haddock -t org --base-header-level=2")))
        (process-send-string pandoc (cl-getf data :content))))))

         ;; (org (elmord-read-command-output
         ;;       "bash" "-c" "xclip -o -t text/html | pandoc -f html -t haddock | pandoc -f haddock -t org --base-header-level=2"))


(defadvice battery-update (around elmord-battery-update activate)
  (let* ((data (and battery-status-function (funcall battery-status-function)))
         (percentage (car (read-from-string (cdr (assq ?p data)))))
         (status (cdr (assq ?B data)))
         (face (cond ((and (numberp percentage)
                           (equal status "Discharging"))
                      (cond ((< percentage battery-load-critical) 'elmord-battery-critical)
                            ((< percentage battery-load-low) 'elmord-battery-low)
                            (t 'elmord-battery-discharging)))
                     ((member status '("Charging" "Full"))
                      'elmord-battery-charging)
                     (t 'elmord-battery-unknown))))
    (setq battery-mode-line-string
          (propertize (format (if (numberp percentage) "[%d%%]" "[%S]") percentage)
                      'face face
                      'help-echo (format "Battery status: %s" status)))))

(defface elmord-battery-charging
  '((t nil)) ;;:background "blue"))
  "Battery charging status")

(defface elmord-battery-discharging
  '((t nil)) ;;:background "yellow"))
  "Battery discharging status")

(defface elmord-battery-low
  '((t :background "orange"))
  "Battery critically low status")

(defface elmord-battery-critical
  '((t :background "red"))
  "Battery critically low status")

(defface elmord-battery-unknown
  '((t nil)) ;;:background "magenta"))
  "Battery critically low status")
