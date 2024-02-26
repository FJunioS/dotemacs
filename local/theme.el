;;; theme.el --  -*- lexical-binding: t -*-

(defgroup koi nil
  "Detailed desc."
  :group 'face
  :prefix "koi-"
  :tag "Koi Theme")

(defgroup koi-faces ()
  "Faces defined by Koi."
  :group 'koi
  :prefix "koi-"
  :tag "Koi Theme Faces")

(deftheme koi
  "Personal scheme of theme."
  :family 'koi
  :background-mode 'dark)

(provide 'theme)
;;; theme.el ends here
