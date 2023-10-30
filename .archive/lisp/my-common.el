;;; my-common.el --- common things
;; Copyright (c) 2021-2023  Junio Santos <info@junio.dev>

;; Author: Junio Santos <info@junio.dev>
;; URL: https://junio.dev/dotemacs
;; Version: 0.2.0
;; Package-Requires: ((emacs "30.1"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO

;;; Code:

(defgroup my-common ()
	"Auxiliary functions."
	:group 'editing)

;; Command Lines

;;; Get files from config folder easier.
;;;###autoload
(defun +append-with-config (path)
	"Function receives a PATH and return assigned on Emacs folder."
	(interactive)
	(let ((path (expand-file-name path user-emacs-directory)))
		path))
;;

;;;###autoload
(defun ju.describe-symbol ()
	"Run `describe-symbol' for the `symbol-at-point'."
	(interactive)
	(describe-symbol (symbol-at-point)))

;;;###autoload
(defmacro after! (feature &rest body)
	`(with-eval-after-load ,feature
		 ,@body))

(defmacro define-key! (map keybind command)
	`(define-key ,map (kbd ,keybind) ',command))

;;;###autoload
(defmacro map! (map &rest bindings)
	`(progn
		 ,@(seq-map (lambda (pair)
						`(define-key! ,map ,(car pair) ,(cadr pair)))
               (seq-partition bindings 2))))

;;;###autoload
(defun jun-new-line-below (&optional arg)
	"Create an empty line below the current one.
Move the point to the absolute beginning.  Adapt indentation by
passing optional prefix ARG (\\[universal-argument]).  Also see
`prot-simple-new-line-above'."
	(interactive "P")
	(end-of-line)
	(if arg
		(newline-and-indent)
		(newline)))

;;;###autoload
(defun jun-new-line-above (&optional arg)
	"Create an empty line above the current one.
Move the point to the absolute beginning.  Adapt indentation by
passing optional prefix ARG (\\[universal-argument])."
	(interactive "P")
	(let ((indent (or arg nil)))
		(if (or (bobp)
				(line-number-at-pos (point-min)))
			(progn
				(beginning-of-line)
				(newline)
				(forward-line -1))
			(forward-line -1)
			(prot-simple-new-line-below indent))))
;;
;;;###autoload
(defun jun/load-module (filename &optional load-module-only)
	"jun/load-module tries to match the argument with one of 
the files inside 'emacs-user-directory/modules' and do `load-file' with it.
Also its complementary files get loaded alltogether. For more info about this
see function `jun/load-addons' down below.
`jun/load-module' will only load the module if invoked with a non-nil argument,
otherwise will load by default.
"
	;; Set all variables
	(let ((msg (list ))
			 (name (symbol-name filename))
			 (path-mod (make-symbol "path-mod"))
			 (path-addon (make-symbol "path-addon")))

		;; load addons
		(unless load-module-only
			(let ((path-addon (concat (expand-file-name name path-lisp) ".el")))
				(when (file-exists-p path-addon)
					(load-file path-addon)
					(push " with addons!" msg))))

		;; load module
		(let ((path-mod$ (concat (expand-file-name name path-modules) ".el")))
			(message path-mod$)
			(if (file-exists-p path-mod$)
				(progn (load-file path-mod$)
					(push (format "Mod: '%s' loaded Ok" name) msg)
					(mapc (lambda (yay)
							  ;; if everything did fine, message user.
							  (message yay)) msg))
				;; In case of any error, warn user.
				(warn "Mod: '%s' did not load. NOT Ok!" name)))))
;;

;;;###autoload
(defun jun/load-addons (&optional --filename)
	"jun/load-addons loads the complementary functions 
from specified file, making use of lisp folder to store those functions.
The main reason is to let configuration files more clean with only the
settings needed and not mixing it with helper methods."
	(let ((name (if --filename ; Parse user argument if any
					(symbol-name --filename)
					;; Otherwise get current buffer filename without any extension
					(file-name-nondirectory (file-name-sans-extension buffer-file-name))))
			 ;; Only declares the `path' variable
			 (path (make-symbol "path")))
		(let ((path (concat (expand-file-name name path-lisp) ".el") ))
			(if (file-exists-p 'path)
				(progn (load-file 'path)
					(message "Addon: '%s' loaded Ok!" name))
				(warn "Addon: '%s' did not load. NOT Ok!" name)))))

;;;;---------
;;;; Editing
;;
;;;###autoload
(defun jun-common-truncate-lines-silently ()
  "Toggle line truncation without printing messages."
  (let ((inhibit-message t))
    (toggle-truncate-lines t)))

;;;###autoload
(defun jun-common-empty-buffer-p ()
  "Test whether the buffer is empty."
  (or (= (point-min) (point-max))
      (save-excursion
        (goto-char (point-min))
        (while (and (looking-at "^\\([a-zA-Z]+: ?\\)?$")
                    (zerop (forward-line 1))))
        (eobp))))

;;;###autoload
(defun jun-common-disable-hl-line ()
  "Disable Hl-Line-Mode (for hooks)."
  (hl-line-mode -1))

;;;###autoload
(defun jun-common-window-bounds ()
  "Determine start and end points in the window."
  (list (window-start) (window-end)))

;;;###autoload
(defun jun-common-minor-modes-active ()
  "Return list of active minor modes for the current buffer."
  (let ((active-modes))
    (mapc (lambda (m)
            (when (and (boundp m) (symbol-value m))
              (push m active-modes)))
          minor-mode-list)
    active-modes))

(defvar jun-common-url-regexp
  (concat
   "~?\\<\\([-a-zA-Z0-9+&@#/%?=~_|!:,.;]*\\)"
   "[.@]"
   "\\([-a-zA-Z0-9+&@#/%?=~_|!:,.;]+\\)\\>/?")
  "Regular expression to match (most?) URLs or email addresses.")

(provide 'my-common)
;; my-common.el ends here
