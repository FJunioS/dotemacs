;;; email.el ---  desc  -*- lexical-binding: t; -*-
;;; Commentary:
;; To setup an email you gonna need something...
;; 1. to fetch (isync, offlineimap)
;; 2. to send (Emacs itself, msmtp)
;; 3. to read (Mutt, Mu4e, Notmuch, Gnus)
;; 4. to manage (afew) /optional/
;;
;; Be clear that many of these tools are binaries and need to be installed on your O.S
;; which, personally, isn't my prefered way of doing things in Emacs,
;; but I already know that handling email clients in a lower level, that is, by not
;; using gmail, thunderbird, etc., can easilly go from a simple task to a true nightmare,
;; because of this, wouldn't be wise to ignore those facilities.
;;
;; But by now I'm looking forwards this solution: https://stalw.art/
;; let's see where this goes.
;;
;;; Code:

;; iSync: A command line application which synchronizes mailboxes.
(use-package notmuch)
(use-package password-store)
(use-package password-store-otp)
(use-package pass)

(provide 'email)
;;; email.el ends here
