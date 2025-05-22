;; The default encoding should be utf-8 everywhere
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)

;; personal.el - personal settings
(setq personal-file "~/.emacs.d/personal.el")
(load personal-file 'noerror)

;; Setup straight.el
;; https://github.com/radian-software/straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)
(package-initialize)
(straight-use-package 'bind-key)

;; Themeing
(use-package modus-themes
  :straight t
  :config
  ;; Load the theme of your choice.
  (if (file-exists-p "~/.emacs.d/dark-mode")
      (load-theme 'modus-vivendi t)
    (load-theme 'modus-operandi t))

  (define-key global-map (kbd "<f5>")
              (lambda ()
                (interactive)
                (let ((dark-mode-file "~/.emacs.d/dark-mode"))
                  (if (file-exists-p dark-mode-file)
                      (delete-file dark-mode-file)
                    (with-temp-buffer
                      (write-file dark-mode-file)))
                  (modus-themes-toggle))))

  ;; Default frame size
  (setq default-frame-alist
        (append (list '(width  . 90) '(height . 50)
                      '(vertical-scroll-bars . nil)
                      '(internal-border-width . 5)))))

;; Disable some UI elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(fringe-mode -1)
;; Font settings
(set-face-attribute 'default nil :font "Inconsolata-10")
(set-face-attribute 'fixed-pitch nil :family 'unspecified)
(set-face-attribute 'variable-pitch nil :family 'unspecified)

;; Icons
(use-package all-the-icons
  :straight t)

;; Rainbow delimiters
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode))

;; Disable backup files
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Dired settings
(defun embark-open-externally (file)
  "Open FILE externally using the default application of the system."
  (interactive "fOpen externally: ")
  (if (and (eq system-type 'windows-nt)
           (fboundp 'w32-shell-execute))
      (w32-shell-execute "open" file)
    (call-process (pcase system-type
                    ('darwin "open")
                    ('cygwin "cygstart")
                    (_ "xdg-open"))
                  nil 0 nil
                  (expand-file-name file))))

(defun dired-open-externally (&optional arg)
  "Open marked or current file in operating system's default application."
  (interactive "P")
  (dired-map-over-marks
   (embark-open-externally (dired-get-filename))
   arg))

(setf dired-kill-when-opening-new-dired-buffer t)
(setq dired-listing-switches "-laGh1v --group-directories-first")
(setq dired-omit-files
      (rx (or (seq bol (? ".") "#")     ;; emacs autosave files
              (seq bol "." (not (any "."))) ;; dot-files
              (seq "~" eol)                 ;; backup-files
              (seq bol "CVS" eol)           ;; CVS dirs
              )))
(defun my-dired-init ()
  "to be run as hook for `dired-mode'."
  (interactive)
  (dired-hide-details-mode 1)
  (dired-omit-mode 1)
  (bind-key "E" 'dired-open-externally))
(add-hook 'dired-mode-hook 'my-dired-init)

;; All "Yes or No" questions can be shortend to "y or n".
(defalias 'yes-or-no-p 'y-or-n-p)

;; No more startup messages and screens
(setq inhibit-startup-screen t)
(setq initial-buffer-choice  nil)
(defun display-startup-echo-area-message ()
  (message "Welcome Back!"))

;; Highlight matching braces
(show-paren-mode t)

;; cua-mode. Like any other editor
(cua-mode t)

;; Configure the cursor
(setq-default
 cursor-type 'bar
 indent-tabs-mode nil
 cursor-in-non-selected-windows nil)
(blink-cursor-mode 0)

;; Default column with
(set-fill-column 95)

;; Start up in the home directory
(setq default-directory "~/")

;; Make C-k always kill the whole line
(setq kill-whole-line t)

;; Do not ding. Ever.
(setq ring-bell-function 'ignore)

;; Dialogues always go in the modeline.
(setq use-dialog-box nil)

;; Show tooltips on hover and not in the echo area.
;; Those are often cut of.
(tooltip-mode)

;; Better line wraping
(global-visual-line-mode 1)

;; Keybindings
(bind-key "C-x k"      'kill-buffer-with-prejudice)
(bind-key "C-w"        'kill-buffer-with-prejudice)
(bind-key "C-x C-k"    'kill-buffer-and-window)
(bind-key "M-i"        'delete-indentation)
(bind-key "C-+"        'text-scale-increase)
(bind-key "C--"        'text-scale-decrease)
(bind-key "C-x C-b"    'ibuffer)
(bind-key "C-c n"      'display-line-numbers-mode)
(bind-key "M-1"        'delete-other-windows)
(bind-key "C-M-1"      'delete-other-windows)
(bind-key "M-0"        'delete-window)
(bind-key "C-S"        'save-buffer)

(global-set-key (kbd "<f12>") 'menu-bar-mode)


;; Edit this file
(defun edit-config ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; Custom functions
(defun lux/indent-buffer ()
  "Reindents the whole buffer"
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key [f1] 'lux/indent-buffer)

(defun lux/split-right-and-enter ()
  "Split the window to the right and enter it."
  (interactive)
  (split-window-right)
  (other-window 1))
(bind-key "M-3" 'lux/split-right-and-enter)

(defun lux/split-below-and-enter ()
  "Split the window down and enter it."
  (interactive)
  (split-window-below)
  (other-window 1))
(bind-key "M-2" 'lux/split-below-and-enter)

(defun switch-to-previous-buffer ()
  "Switch to previously open buffer.Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))
(bind-key "M-p"        'switch-to-previous-buffer)

;; Ace for qucik window switching
(use-package ace-window
  :straight t
  :bind (("M-o" . 'ace-window))
  :config
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:inherit ace-jump-face-foreground :height 3.0))))
   ))

(use-package multiple-cursors
  :straight t
  :bind
  (("C-S-c C-S-c" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)))
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; Ivy completion
(use-package ivy
  :straight t
  :diminish
  :init
  (ivy-mode 1)
  (unbind-key "S-SPC" ivy-minibuffer-map)
  (setq ivy-height 30
        ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t)
  :bind (("C-x b"   . ivy-switch-buffer)
         ("C-c C-r" . ivy-resume)
         ("C-M-s"     . swiper)))

;; ivy-rich makes Ivy look a little bit more like Helm.
(use-package ivy-rich
  :straight t
  :after counsel
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :init
  (ivy-rich-mode))

(use-package ivy-hydra :straight t)

;; smex for better M-x
(use-package smex :straight t)

(use-package counsel
  :straight t
  :after ivy
  :init (counsel-mode 1)
  :bind (("C-c ;" . counsel-M-x)
         ("C-c U" . counsel-unicode-char)
         ("C-c i" . counsel-imenu)
         ("C-c y" . counsel-yank-pop)
         ("C-c r" . counsel-recentf)
         :map ivy-minibuffer-map
         ("C-r" . counsel-minibuffer-history))
  :diminish)

(use-package ido
  :straight t
  :config (ido-mode 1)
  :bind (("C-x f" . ido-find-file)))

(use-package auto-complete
  :straight t
  :config
  (ac-config-default))

;; Magit - git client
(use-package magit
  :straight t
  :bind (("C-c g" . magit-status))
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :custom
  (magit-remote-set-if-missing t)
  (magit-diff-refine-hunk t)
  :config
  (magit-auto-revert-mode t)
  (advice-add 'magit-refresh :before #'maybe-unset-buffer-modified)
  (advice-add 'magit-commit  :before #'maybe-unset-buffer-modified)
  (setq magit-completing-read-function 'ivy-completing-read)
  (add-to-list 'magit-no-confirm 'stage-all-changes))

(use-package libgit
  :straight t
  :disabled
  :after magit)

;; Duplicate
(use-package duplicate-thing
  :straight t
  :bind (("C-c u" . duplicate-thing)))

;; Markdown
(use-package markdown-mode
  :straight t
  :mode ("\\.md$" . gfm-mode)
  :config
  (when (executable-find "pandoc")
    (setq markdown-command "pandoc -f markdown -t html")))

;; Org mode
(use-package org-modern
  :straight t
  :hook (org-mode . org-modern-mode))

(setq
 ;; Edit settings
 org-auto-align-tags nil
 org-tags-column 0
 org-catch-invisible-edits 'show-and-error
 org-special-ctrl-a/e t
 org-insert-heading-respect-content t

 ;; Org styling, hide markup etc.
 org-hide-emphasis-markers t
 org-pretty-entities t
 )

;; Always collapse org files
(setq org-startup-folded t)

;; Programming configuration
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Copilot
;; (use-package copilot
;;   :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
;;   :ensure t)
;; (add-hook 'prog-mode-hook 'copilot-mode)
;; (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
;; (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
;; (define-key copilot-completion-map (kbd "C-.") 'copilot-complete)

;; Elisp
(defun my-elisp-mode-hook ()
  "My elisp customizations."
  (electric-pair-local-mode 1)
  (add-hook 'before-save-hook 'check-parens nil t)
  (auto-composition-mode nil))
(add-hook 'emacs-lisp-mode-hook 'my-elisp-mode-hook)

;; Common Lisp
(setq inferior-lisp-program "sbcl")
(use-package slime
  :straight t)
(slime-setup '(slime-fancy slime-quicklisp slime-asdf))
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

;; Misc functions

(autoload 'diff-no-select "diff")
(defun current-buffer-matches-file-p ()
  "Return t if the current buffer is identical to its associated file."
  (when (and buffer-file-name (buffer-modified-p))
    (diff-no-select buffer-file-name (current-buffer) nil 'noasync)
    (with-current-buffer "*Diff*"
      (and (search-forward-regexp "^Diff finished \(no differences\)\." (point-max) 'noerror) t))))

(defun maybe-unset-buffer-modified (&optional _)
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and buffer-file-name (buffer-modified-p) (current-buffer-matches-file-p))
        (set-buffer-modified-p nil)))))

(defun kill-buffer-with-prejudice (&optional _)
  "Kill a buffer, eliding the save dialogue if there are no diffs."
  (interactive)
  (when (current-buffer-matches-file-p) (set-buffer-modified-p nil))
  (kill-buffer))

;; custom.el - device specific settings
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(provide 'init)
