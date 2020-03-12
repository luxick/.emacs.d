;; init.el -- Marcel Fries <mfries@luxick.de>

;; Configure emacs in a literate style from an org file.
;; The file sould be named "setup-emacs.org". It should be located in your ".emacs.d" directory.
;; Source at https://git.sr.ht/~luxick/emacs-config

(require 'org)
(org-babel-load-file
 (expand-file-name (concat user-emacs-directory "README.org")))

(provide 'init)
