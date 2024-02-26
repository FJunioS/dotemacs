;;; window.el ---  desc  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(require 'popup-handler)
(require 'core-lib)

(noct-handle-popup (rx "*Warnings*"))
(noct-handle-popup (rx "*eldoc*"))
(noct-handle-popup (rx "*Flycheck errors*"))

(cl-pushnew
 (list (rx "*Async Shell Command*" (0+ any)) #'display-buffer-no-window)
 display-buffer-alist)

(provide 'window)
;;; window.el ends here
