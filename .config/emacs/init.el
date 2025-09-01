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

(setq straight-use-package-by-default t)
(setq straight-vc-git-default-clone-depth 1) ; shallow clone
(setq straight-check-for-modifications '(check-on-save find-when-checking))

;; Disable native-compilation at install time to avoid slowdowns
(setq straight-disable-native-compile t)

(straight-use-package 'use-package)
(require 'use-package)

(when (and (fboundp 'native-compile)
           (boundp 'native-comp-deferred-compilation))
  ;; Enable background compilation AFTER loading packages
  (setq native-comp-deferred-compilation t)
  (setq native-comp-jit-compilation t)
  (setq comp-async-report-warnings-errors nil)

  ;; Dynamically detect CPU count to set job number
  (when (boundp 'comp-async-jobs-number)
    (let* ((cpu-count
            (cond
             ((executable-find "nproc")
              (string-to-number
               (string-trim (shell-command-to-string "nproc"))))
             ((eq system-type 'darwin)
              (string-to-number
               (string-trim (shell-command-to-string "sysctl -n hw.ncpu"))))
             (t 1)))
           (jobs (min cpu-count 8))) ; limit max jobs to avoid overheating
      (setq comp-async-jobs-number (max jobs 1)))))

;; Store .eln files in a custom cache directory
(when (boundp 'native-comp-eln-load-path)
  (add-to-list 'native-comp-eln-load-path
               (expand-file-name "eln-cache/" user-emacs-directory)))

;; General Settings
(setq inhibit-startup-message t                        ; Disable splash screen
      initial-scratch-message nil                      ; Empty scratch buffer
      delete-by-moving-to-trash t                      ; Use system trash on delete
      make-backup-files t                              ; Enable backups
      backup-by-copying t                              ; Copy files for backup (safer for symlinks)
      require-final-newline t                          ; Add newline at end of file on save
      load-prefer-newer t                              ; Prefer newer versions of files
      apropos-do-all t                                 ; Show all results in apropos
      mouse-yank-at-point t                            ; Yank at point, not click location
      read-file-name-completion-ignore-case t          ; Ignore case in file name completion
      read-buffer-completion-ignore-case t             ; Ignore case in buffer name completion
      vc-follow-symlinks t                             ; Follow symlinks without confirmation
      size-indication-mode t                           ; Show file size in mode line
      browse-url-browser-function 'browse-url-firefox) ; Open URL links using firefox



(setq-default
 indent-tabs-mode nil            ; Use spaces instead of tabs
 tab-width 2                     ; Set tab width to 2
 fill-column 80                  ; Set fill column
 truncate-lines nil              ; Don't truncate lines
 word-wrap t                     ; Wrap at word boundaries
 sentence-end-double-space nil)  ; Single space after sentences

;; UI Tweaks
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(line-number-mode 1)
(column-number-mode 1)

;; Behavior
(global-visual-line-mode 1)     ; Soft-wrap lines
(global-auto-revert-mode 1)     ; Auto-refresh changed files
(savehist-mode 1)               ; Save minibuffer history
(recentf-mode 1)                ; Recent files
(delete-selection-mode 1)       ; Replace selection when typin

;; Parentheses
(show-paren-mode 1)             ; Highlight matching parentheses
(setq show-paren-delay 0)
(setq show-paren-style 'mixed)
(electric-pair-mode 1)          ; Auto-insert closing delimiters

;; Line Numbers
(setq display-line-numbers-type 'relative)  ; Relative line numbers
(global-display-line-numbers-mode 1)

;; Load extra config
(let ((modules-dir (expand-file-name "extra-config" user-emacs-directory)))
  (when (file-directory-p modules-dir)
    (dolist (file (directory-files modules-dir t "\\.el$"))
      (condition-case err
          (load file)
        (error (message "Error loading %s: %s" file err))))))

;; Themes
(use-package gruber-darker-theme)
(use-package gruvbox-theme)

(load-theme 'gruvbox-dark-soft t)

;; Fonts
;;(set-frame-font "Iosevka Nerd Font 16" nil t)
;; (set-face-attribute 'default nil
;;                     :font "Iosevka Nerd Font"
;;                     :height 160) ;; 160 = 16pt
(add-to-list 'default-frame-alist '(font . "Aporetic Serif Mono-14"))

;; Whitespaces
;; TODO is not done yet
;; GPT say is better put inside of use-package so...
(use-package whitespace
  :config
  (setq whitespace-style
        '(face tabs tab-mark
               indentation indentation::tab indentation::space
               space-before-tab space-before-tab::tab space-before-tab::space
               space-after-tab space-after-tab::tab space-after-tab::space
               trailing lines-tail
               missing-newline-at-eof))

  (setq whitespace-display-mappings
        '((space-mark ?\xA0 [?⍽])
          (tab-mark   ?\t  [?» ?\t])))

  ;; Custom face for whitespace
  (set-face-attribute 'whitespace-tab nil
                      :foreground "#fb4933" :background "#3c3836")

  :hook ((prog-mode . whitespace-mode)
         (text-mode . whitespace-mode)))

;; Function to delete trailing whitespace on save
(defun user/delete-trailing-whitespace-on-save ()
  "Add hook to delete trailing whitespace before saving."
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(add-hook 'prog-mode-hook #'user/delete-trailing-whitespace-on-save)
(add-hook 'text-mode-hook #'user/delete-trailing-whitespace-on-save)

;; Dired
(add-hook 'dired-mode-hook 'auto-revert-mode) ; Auto refresh if dir changes

;; Tree-sitter
;; Arch install grammars in /usr/lib
(setq treesit-extra-load-path '("/usr/lib"))

;; Use TS by default
(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (python-mode . python-ts-mode)
        (c-mode . c-ts-mode)
        (json-mode . json-ts-mode)
        (javascript-mode . js-ts-mode)))

;; Undo-tree
(use-package undo-tree
  :config
  (global-undo-tree-mode))


;; Which-key
(use-package which-key
  :config
  (which-key-mode 1))

;; Vertico
(use-package vertico
  :demand t
  :config
  (vertico-mode)
  (setq vertico-cycle t
        vertico-resize nil
        vertico-count 15))

;; Marginalia
(use-package marginalia
  :config
  (marginalia-mode 1))

;; Orderless
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))))

;; Consult
(use-package consult)

;; Embark
(use-package embark
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings))
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Avy
(use-package avy
  :bind (("C-'" . avy-goto-char)
         ("C-c j" . avy-goto-line)))

;; Wgrep
(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

;; Magit
(use-package magit
  :defer t)

;; Rainbow-delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Company
(use-package company
  :hook (after-init . global-company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("C-f" . company-complete-selection))
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t
        company-selection-wrap-around t
        company-show-numbers t
        company-backends '((company-capf company-dabbrev-code company-keywords)
                          company-dabbrev)))

;; Move-text
(use-package move-text
  :config
  (move-text-default-bindings) ; M-up / M-down
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down)))

;; Markdown
(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "pandoc"
        markdown-fontify-code-blocks-natively t))

;; Multiple cursor
;; Read docs https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("C-\""        . mc/skip-to-next-like-this)
         ("C-:"         . mc/skip-to-previous-like-this)))

;; Colorful-mode
(use-package colorful-mode
  :hook prog-mode
  :config
  (setq colorful-use-prefix t
        colorful-only-strings 'only-prog
        css-fontify-colors nil)
  (global-colorful-mode t))

;; Vterm
(use-package vterm
  :bind ("C-c t" . user/vterm-here)
  :config
  (setq vterm-max-scrollback 10000
        vterm-kill-buffer-on-exit t)

  (let* ((vterm-repo-dir (straight--repos-dir "emacs-libvterm"))
         (module-path (expand-file-name "vterm/vterm-module.so" vterm-repo-dir)))
    (unless (file-exists-p module-path)
      (message "Compiling vterm-module...")
      (let ((default-directory vterm-repo-dir))
        (when (and (file-exists-p "CMakeLists.txt")
                   (executable-find "cmake")
                   (executable-find "make"))
          (shell-command "cmake . && make"))))
    (when (file-exists-p module-path)
      (load module-path nil t))))

(defun user/vterm-here ()
  "Open vterm in the current buffer's directory."
  (interactive)
  (let ((default-directory (or (and buffer-file-name
                                    (file-name-directory buffer-file-name))
                               default-directory)))
    (vterm)))

;; Yasnippet
(use-package yasnippet
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs '("~/.config/emacs/snippets")) ; Custom snippets directory
  (setq yas-prompt-functions '(yas-dropdown-prompt yas-completing-prompt))
  (setq yas-wrap-around-region t) ; Allow wrapping text with snippets
  (setq yas-triggers-in-field t)) ; Don't ask before expanding snippets

(use-package yasnippet-snippets
  :after yasnippet)

(use-package rfc-mode
  :config
  (setq rfc-mode-directory (expand-file-name "~/.rfc/")))
