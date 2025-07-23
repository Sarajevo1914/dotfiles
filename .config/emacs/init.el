;;; init.el --- Init -*- lexical-binding: t; -*-
;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package and enable for straight
(straight-use-package 'use-package)
(require 'use-package)
(setq straight-use-package-by-default t)

;; General Settings
(setq inhibit-startup-message t               ; Disable splash screen
      delete-by-moving-to-trash t             ; Use system trash on delete
      make-backup-files t                     ; Enable backups
      backup-by-copying t                     ; Copy files for backup (safer for symlinks)
      require-final-newline t                 ; Add newline at end of file on save
      load-prefer-newer t                     ; Prefer newer versions of files
      apropos-do-all t                        ; Show all results in apropos
      mouse-yank-at-point t                   ; Yank at point, not click location
      read-file-name-completion-ignore-case t ; Ignore case in file name completion
      read-buffer-completion-ignore-case t    ; Ignore case in buffer name completion
      vc-follow-symlinks t                    ; Follow symlinks without confirmation
      tab-width 2                             ; Set tab width to 2
      size-indication-mode t                  ; Show file size in mode line
      global-recentf-mode t)                  ; Enable recent files list

(setq-default indent-tabs-mode nil)           ; Use spaces instead of tabs

;; UI Tweaks
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(line-number-mode 1)
(column-number-mode 1)

;; Behavior
(global-visual-line-mode 1)                  ; Soft-wrap lines
(global-auto-revert-mode 1)                  ; Auto-refresh changed files
(savehist-mode 1)                            ; Save minibuffer history

;; Parentheses
(show-paren-mode 1)                          ; Highlight matching parentheses
(setq show-paren-delay 0)
(electric-pair-mode 1)                       ; Auto-insert closing delimiters

;; Line Numbers
(setq display-line-numbers-type 'relative)  ; Relative line numbers
(global-display-line-numbers-mode 1)

;; Themes
(use-package gruber-darker-theme)
(use-package gruvbox-theme)

(load-theme 'gruvbox-dark-soft t)

;; Fonts
;;(set-frame-font "Iosevka Nerd Font 16" nil t)
;; (set-face-attribute 'default nil
;;                     :font "Iosevka Nerd Font"
;;                     :height 160) ;; 160 = 16pt
(add-to-list 'default-frame-alist '(font . "Iosevka Nerd Font-14"))

;; Whitespaces
;; TODO is not done yet
;; GPT say is better put inside of use-package so...
(use-package whitespace
  :init
  (setq whitespace-style
        '(face tabs tab-mark
               indentation indentation::tab indentation::space
               space-before-tab space-before-tab::tab space-before-tab::space
               space-after-tab space-after-tab::tab space-after-tab::space
               trailing line lines-tail
               missing-newline-at-eof))

  (setq whitespace-display-mappings
        '((space-mark ?\xA0 [?⍽])
          (tab-mark   ?\t  [?» ?\t])))

  :hook ((prog-mode . whitespace-mode)
         (text-mode . whitespace-mode))

  :config
  (set-face-attribute 'whitespace-tab nil
    :foreground "#fb4933" :background "#3c3836")

  ;(set-face-attribute 'whitespace-space nil
   ; :foreground "#fabd2f" :background "#3c3836")
  )

(defun user/delete-trailing-whitespace-on-save ()
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(add-hook 'prog-mode-hook #'user/delete-trailing-whitespace-on-save)
(add-hook 'text-mode-hook #'user/delete-trailing-whitespace-on-save)

;; Dired
(add-hook 'dired-mode-hook 'auto-revert-mode) ; Auto refresh if dir chage

;; Which-key
(which-key-mode 1)

;; Vertico
(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t
        vertico-resize nil))

;; Marginalia
(use-package marginalia
  :config
  (marginalia-mode 1))

;; Orderless
(use-package orderless
  :config
  (setq completion-styles '(orderless)))

;; Consult
(use-package consult)

;; Embark
(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult)

;; Wgrep
(use-package wgrep)

;; Magit
(use-package magit
  :defer t)

;; Rainbow-delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Company
(use-package company
  :init
  (global-company-mode 1)
  :config
(setq company-idle-delay 0.2
      company-minimum-prefix-length 1
      company-tooltip-align-annotations t
      company-selection-wrap-around t))

;; Move-text
(use-package move-text
  :config
  (move-text-default-bindings)) ; M-up / M-down
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

;; Markdown
(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode))
  :init
  (setq markdown-command "pandoc"))

;; Multiple cursor
;; Read docs https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

;; Colorful-mode
(use-package colorful-mode
  :custom
  (colorful-use-prefix t)
  (colorful-only-strings 'only-prog)
  (css-fontify-colors nil)
  :config
  (global-colorful-mode t))

;; Vterm
(use-package vterm
  :config
  (setq vterm-max-scrollback 100000))

(defun user/vterm-here ()
  "Open vterm in the current buffer's directory."
  (interactive)
  (let ((default-directory (or (and (buffer-file-name)
                                    (file-name-directory (buffer-file-name)))
                               default-directory)))
    (vterm)))

(global-set-key (kbd "C-c t") 'user/vterm-here)
;(global-set-key (kbd "C-t") 'vterm-toggle)
