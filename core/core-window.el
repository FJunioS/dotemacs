;; -*- lexical-binding: t; -*-

(defmacro core-match-major-mode (mode)
  "Create a function that return whether the current `major-mode' is MODE."
  (let ((name (intern (format "core-match-%s" mode))))
    `(progn
       (defun ,name (buffer-or-name _action)
         (ignore-errors
           (let ((buffer (get-buffer buffer-or-name)))
             (eq ',mode (buffer-local-value 'major-mode buffer)))))
       #',name)))

(defun core-display-and-select-buffer (func buffer alist)
  "Call FUNC with BUFFER and ALIST.
Select the window afterwards if possible. This is modified from
`shackle--display-buffer-reuse'. Additionally set the window to be fixed size."
  (let ((window (funcall func buffer alist)))
    (when (and window (window-live-p window))
      (select-window window t))
    ;; TODO this breaks slots; doesn't work for non-side windows
    ;; (with-current-buffer buffer
    ;;   (setq window-size-fixed t))
    window))

(defun core-display-buffer-reuse-window (buffer alist)
  "Call `display-buffer-reuse-window' with BUFFER and ALIST.
Select the window afterwards if possible."
  (core-display-and-select-buffer #'display-buffer-reuse-window buffer alist))

(defun core-display-buffer-in-side-window (buffer alist)
  "Call `display-buffer-in-side-window' with BUFFER and ALIST.
Select the window afterwards if possible."
  (core-display-and-select-buffer #'display-buffer-in-side-window buffer alist))

(defun core-display-buffer-in-side-window-no-header (buffer alist)
  "`core-display-buffer-in-side-window' but don't have a header line.
Having a header line in some buffers will cause text to be cut off at the
bottom (e.g. transient and frog menu)."
  (core-display-buffer-in-side-window buffer alist)
  (setf (buffer-local-value 'header-line-format buffer) nil))

(defun core-display-buffer-same-window (buffer alist)
  "Call `display-buffer-same-window' with BUFFER and ALIST.
Select the window afterwards if possible."
  (core-display-and-select-buffer #'display-buffer-same-window buffer alist))

(defun shackle--split-some-window (frame alist)
  "Return a window if splitting any window was successful.
This function tries using the largest window on FRAME for
splitting, if all windows are the same size, the selected one is
taken, in case this fails, the least recently used window is used
for splitting.  ALIST is passed to `window--try-to-split-window'
internally."
  (or (window--try-to-split-window (get-largest-window frame t) alist)
      (window--try-to-split-window (get-lru-window frame t) alist)))

(defun shackle--display-buffer-popup-window (buffer alist)
  "Display BUFFER in a popped up window.
This is a stripped down version of `shackle--display-buffer-popup-window'.
ALIST is passed to `shackle--window-display-buffer' internally.
If PLIST contains the :other key with t as value, reuse the next
available window if possible."
  (let ((window (if (not (one-window-p))
                    (next-window nil 'nominibuf)
                  (shackle--split-some-window (selected-frame) alist))))
    (window--display-buffer buffer window 'window alist)))

(defun core-display-buffer-creating-other-window (buffer alist)
  "Call `display-buffer-in-other-window' with BUFFER and ALIST.
If another window does not exist, create it. Select the window afterwards if
possible."
  (core-display-and-select-buffer #'shackle--display-buffer-popup-window
                                  buffer alist))

(defmacro core-handle-window (condition &rest body)
  "Display windows matching CONDITION with the settings in BODY."
  (declare (indent 1) (debug t))
  (let ((condition (if (and (symbolp condition)
                            (string-match "-mode$" (symbol-name condition)))
                       `(core-match-major-mode ,condition)
                     condition)))
    `(cl-pushnew
      (list ,condition ,@body)
      display-buffer-alist
      :test 'equal)))

(defmacro core-handle-popup (condition &optional slot)
  "Display popups matching CONDITION in a side window at the top.
When SLOT is non-nil, display popup buffers in that SLOT in the side window."
  `(core-handle-window ,condition
     '(core-display-buffer-reuse-window core-display-buffer-in-side-window)
     '(side . bottom)
     '(slot . ,slot)
     '(window-height . 0.4)))

(defmacro +handle-popup (condition &optional aside height)
  "Display popups matching CONDITION in a side window at the top.
When SLOT is non-nil, display popup buffers in that SLOT in the side window."
  `(core-handle-window ,condition
     '(core-display-buffer-reuse-window core-display-buffer-in-side-window)
     '(side . ,(if aside 'right 'bottom))
     '(window-height . ,(if height height 0.4))))

(defmacro core-handle-popup-aside-right (condition &optional slot)
  "Display popups matching CONDITION in a side window at the top.
When SLOT is non-nil, display popup buffers in that SLOT in the side window."
  `(core-handle-window ,condition
     '(core-display-buffer-reuse-window core-display-buffer-in-side-window)
     '(side . right)
     '(slot . ,slot)
     '(window-height . 0.4)))

(defmacro core-handle-popup-no-header (condition &optional slot)
  "Display popups matching CONDITION in a side window at the top.
Remove the header line, this handles some buffers where text
would be cut off when there is a header line.  When SLOT is
non-nil, display popup buffers in that SLOT in the side window."
  `(core-handle-window ,condition
     '(core-display-buffer-reuse-window
       core-display-buffer-in-side-window-no-header)
     '(side . bottom)
     '(slot . ,slot)
     '(window-height . 0.4)))

(defmacro core-handle-popup-same-window (condition)
  "Display popups matching CONDITION in the current window."
  `(core-handle-window ,condition
     '(core-display-buffer-reuse-window core-display-buffer-same-window)))

(defmacro core-handle-popup-other-window (condition)
  "Display popups matching CONDITION in the other window.
Create another window if one doesn't exist"
  `(core-handle-window ,condition
     '(core-display-buffer-reuse-window
       core-display-buffer-creating-other-window)))

(defmacro core-handle-popup-other-window-no-select (condition)
  "Display popups matching CONDITION in the other window without selecting it.
Create another window if one doesn't exist"
  `(core-handle-window ,condition
     'shackle--display-buffer-popup-window))

(defun side-window-p ()
  "Return non-nil if the selected window is a side window."
  (window-parameter (selected-window) 'window-side))

(provide 'core-window)
;;; core-window.el ends here.
