;;; vanilla.el --- Vanilla Emacs configuration -*- lexical-binding: t; -*-

;; General settings
(setq
 inhibit-startup-message t                        ; Disable splash screen
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
 sentence-end-double-space nil)  ; Single space after sentences

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

;; Fonts
;;(set-frame-font "Iosevka Nerd Font 16" nil t)
;; (set-face-attribute 'default nil
;;                     :font "Iosevka Nerd Font"
;;                     :height 160) ;; 160 = 16pt
(add-to-list 'default-frame-alist '(font . "Aporetic Serif Mono-14"))

;; Some Keys
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "M-c") 'capitalize-dwim)
(global-set-key (kbd "C-c t") #'toggle-case)

;; Whitespace
(use-package whitespace
  :config
  ;; Define which whitespace to highlight
  (setq whitespace-style
        '(face tabs tab-mark
               indentation indentation::tab indentation::space
               space-before-tab space-before-tab::tab space-before-tab::space
               space-after-tab space-after-tab::tab space-after-tab::space
               trailing lines-tail
               missing-newline-at-eof))

  ;; Display mappings for whitespace characters
  (setq whitespace-display-mappings
        '((space-mark ?\xA0 [?⍽])
          (tab-mark   ?\t  [?» ?\t])))

  ;; Customize tab face
  (set-face-attribute 'whitespace-tab nil
                      :foreground "#fb4933" :background "#3c3836")

  ;; Enable in programming and text modes
  :hook ((prog-mode . whitespace-mode)
         (text-mode . whitespace-mode)))

;; Delete trailing whitespace on save
(defun user/delete-trailing-whitespace-on-save ()
  "Add hook to delete trailing whitespace before saving."
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(add-hook 'prog-mode-hook #'user/delete-trailing-whitespace-on-save)
(add-hook 'text-mode-hook #'user/delete-trailing-whitespace-on-save)

;; Dired
(add-hook 'dired-mode-hook 'auto-revert-mode) ; Auto-refresh dired buffers

;; Tree-sitter (built-in integration)
(setq treesit-extra-load-path '("/usr/lib"))

;; Remap major modes to tree-sitter versions
(setq major-mode-remap-alist
      '((bash-mode . bash-ts-mode)
        (python-mode . python-ts-mode)
        (c-mode . c-ts-mode)
        (json-mode . json-ts-mode)
        (javascript-mode . js-ts-mode)))

(provide 'vanilla)
;;; vanilla.el ends here
