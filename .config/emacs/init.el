;;; init.el --- Vanilla Emacs configuration -*- lexical-binding: t; -*-

;; General settings
(setq
 inhibit-startup-message t                        ; Disable splash screen
 initial-scratch-message nil                      ; Empty scratch buffer
 delete-by-moving-to-trash t                      ; Use system trash on delete
 make-backup-files t                              ; Enable backups
 backup-by-copying t                              ; Copy files for backup (safer for symlinks)

 load-prefer-newer t                              ; Prefer newer versions of files
 apropos-do-all t                                 ; Show all results in apropos
 mouse-yank-at-point t                            ; Yank at point, not click location
 read-file-name-completion-ignore-case t          ; Ignore case in file name completion
 read-buffer-completion-ignore-case t             ; Ignore case in buffer name completion
 vc-follow-symlinks t                             ; Follow symlinks without confirmation
 size-indication-mode t                           ; Show file size in mode line
 browse-url-browser-function 'browse-url-firefox  ; Open URL links using firefox
 my-emacs-trash-dir "~/.cache/emacs/"             ; Set trash dir
 backup-directory-alist `((".*" . ,my-emacs-trash-dir))           ; Backups files
 auto-save-file-name-transforms `((".*" ,my-emacs-trash-dir t))   ; Auto-save files
 auto-save-list-file-prefix (concat my-emacs-trash-dir ".saves-") ; Auto-save crash recovery
 lock-file-name-transforms `((".*" ,my-emacs-trash-dir))          ; Lockfiles
)

(make-directory my-emacs-trash-dir t) ; Create if not exist

;; Save TRAMP files in trash
(setq tramp-persistency-file-name
      (concat my-emacs-trash-dir "tramp"))

;; Default buffer settings
(setq-default
 indent-tabs-mode nil            ; Use spaces instead of tabs
 tab-width 2                     ; Tab width
 fill-column 80                  ; Fill column
 sentence-end-double-space nil   ; Single space after sentences
 require-final-newline t         ; Add newline at end of file on save
 )

;; UI Tweaks
(menu-bar-mode -1)   ; Disable menu bar
(tool-bar-mode -1)   ; Disable tool bar
(scroll-bar-mode -1) ; Disable scroll bar
(line-number-mode 1) ; Enable line numbers in mode line
(column-number-mode 1)

;; Behavior
(global-visual-line-mode 1)  ; Soft-wrap lines
(global-auto-revert-mode 1)  ; Auto-revert changed files
(savehist-mode 1)            ; Save minibuffer history
(recentf-mode 1)             ; Recent files mode
(delete-selection-mode 1)    ; Replace selection when typing

;; Parentheses and delimiters
(show-paren-mode 1)            ; Highlight matching parentheses
(setq show-paren-delay 0)
(setq show-paren-style 'mixed)
(electric-pair-mode 1)         ; Automatically insert closing delimiters

;; Line numbers
(setq display-line-numbers-type 'relative)  ; Relative line numbers
(global-display-line-numbers-mode 1)

;; Font
(add-to-list 'default-frame-alist '(font . "Aporetic Serif Mono-12"))

;; Replace keysbiding
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)
(global-set-key (kbd "C-c t") #'toggle-case)

;; Tree-sitter (built-in integration)
(setq treesit-extra-load-path '("/usr/lib"))

;; Remap major modes to tree-sitter versions
(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (python-mode . python-ts-mode)
        (c-mode . c-ts-mode)
        (json-mode . json-ts-mode)
        (javascript-mode . js-ts-mode)))

;; Dired
(add-hook 'dired-mode-hook 'auto-revert-mode) ; Auto-refresh dired buffers

;; Defer gnus
(use-package gnus
  :defer t)

;; Whitespace

;; Delete trailing whitespace on save
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Load time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s" (emacs-init-time))
            (when (profiler-running-p)
              (profiler-stop)
              (profiler-report))))

;;; init.el ends here
