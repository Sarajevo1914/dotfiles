;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Always use straight with use-package
(setq straight-use-package-by-default t)

;; Install use-package
(straight-use-package 'use-package)
(require 'use-package)

;; Suppress native compile warnings (optional)
(setq native-comp-async-report-warnings-errors nil)

;; Install pkg
(use-package consult)
(use-package vertico :init (vertico-mode))
(use-package marginalia :init (marginalia-mode))
(use-package magit
  :defer t)
(use-package gruber-darker-theme)
(use-package tao-theme)
(use-package ample-theme)
(use-package gruvbox-theme)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; general settings

;; theme
(load-theme 'gruvbox-dark-soft t)

;; font
(when (member "Iosevka Nerd Font" (font-family-list))
  (add-to-list 'default-frame-alist '(font . "Iosevka Nerd Font-16"))
  (set-face-attribute 'default nil :family "Iosevka Nerd Font" :height 160))

(setq inhibit-startup-message t) ; disable default splash screen
(menu-bar-mode -1) ; disable bar
(tool-bar-mode -1) ; disable toolbar
(scroll-bar-mode -1) ; disable scroll bar
(line-number-mode 1) ; show line number in minibuffer
(column-number-mode 1) ; show column number

;; show parent () [] {}
(show-paren-mode 1)
(setq show-paren-delay 0)
(electric-pair-mode 1)

;; line and relative numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; whitespaces
(defun user/set-up-whitespace-handling ()
  (interactive)
  (whitespace-mode 1)
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

;; dired
(add hook 'dired-mode-hook 'auto-reverse-mode) ; auto refresh dir when file change

