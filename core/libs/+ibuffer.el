;;; +ibuffer.el ---  desc  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'ibuffer)
(setq ibuffer-saved-filter-groups
      '(("default"
         ;; ("Modified" (predicate buffer-modified-p (current-buffer)))
         ("Dired" (mode . dired-mode))
         ("Notes" (or (filename . "^~/sync/")
                      (filename . "^~/docs/")))
         ("Config" (or (filename . "^.+/\\.config/emacs/.+$")
                       (filename . "^.+/\\~/sync/config/.+$")))
         ("Magit" (name . "magit\\*"))
         ("Rust" (predicate . (and (eq major-mode 'rust-mode)
                                   (not (string-match-p "spec\\.rs\\'"
                                                        buffer-file-name)))))
         ("Tests" (name . "spec\\.rs\\'"))

         ("Help" (or (mode . helpful-mode)
                     (mode . help-mode)))

         ("Gnus" (or
                  (mode . message-mode)
                  (mode . bbdb-mode)
                  (mode . mail-mode)
                  (mode . gnus-group-mode)
                  (mode . gnus-summary-mode)
                  (mode . gnus-article-mode)
                  (name . "^\\.bbdb$")
                  (name . "^\\.newsrc-dribble")))
         ("*internal*" (name . "\\*"))))

      ;; Modify the default ibuffer-formats
      ibuffer-formats
      '((mark modified read-only locked " "
              (name 20 20 :left :elide)
              " "
              (filename-and-process 40 -1)
              " "
              (+jun/size-h 12 -1 :right :elide)
              "| "
              (mode 20 -1))
        (mark " "
              (name 20 6)
                 " " filename)))

(setq mp/ibuffer-collapsed-groups (list "Emacs" "*Internal*"))

(defadvice ibuffer (after collapse-helm)
  (dolist (group mp/ibuffer-collapsed-groups)
    (progn
      (goto-char 1)
      (when (search-forward (concat "[ " group " ]") (point-max) t)
        (progn
          (move-beginning-of-line nil)
          (ibuffer-toggle-filter-group)))))
  (goto-char 1)
  (search-forward "[ " (point-max) t)
  )

(ad-activate 'ibuffer)

;; Use human readable Size column instead of original one
(defun --human-readable-file-sizes-to-bytes (string)
  "Convert a human-readable file size into bytes."
  (interactive)
  (cond
   ((string-suffix-p "G" string t)
    (* 1000000000 (string-to-number (substring string 0 (- (length string) 1)))))
   ((string-suffix-p "M" string t)
    (* 1000000 (string-to-number (substring string 0 (- (length string) 1)))))
   ((string-suffix-p "K" string t)
    (* 1000 (string-to-number (substring string 0 (- (length string) 1)))))
   (t
    (string-to-number (substring string 0 (- (length string) 1))))));;

(defun --bytes-to-human-readable-file-sizes (bytes)
  "Convert number of bytes to human-readable file size."
  (interactive)
  (cond
   ((> bytes 1000000000) (format "%10.1fG" (/ bytes 1000000000.0)))
   ((> bytes 100000000) (format "%10.0fM" (/ bytes 1000000.0)))
   ((> bytes 1000000) (format "%10.1fM" (/ bytes 1000000.0)))
   ((> bytes 100000) (format "%10.0fk" (/ bytes 1000.0)))
   ((> bytes 1000) (format "%10.1fk" (/ bytes 1000.0)))
   (t (format "%10d" bytes))));;

(define-ibuffer-column +jun/size-h
  (:name "Size"
         :inline t
         :summarizer
         (lambda (column-strings)
           (let ((total 0))
             (dolist (string column-strings)
               (setq total
                     ;; like, ewww ...
                     (+ (float (--human-readable-file-sizes-to-bytes string))
                        total)))
             (--bytes-to-human-readable-file-sizes total))))
  (--bytes-to-human-readable-file-sizes (buffer-size)))

(provide '+ibuffer)

;;; +ibuffer.el ends here
