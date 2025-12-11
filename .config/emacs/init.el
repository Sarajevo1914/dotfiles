;;; init.el --- Vanilla Emacs configuration -*- lexical-binding: t; -*-

;;; Start of elpaca installer

(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; updated org
(elpaca org)
(elpaca-wait)

;; use-package integration
(elpaca elpaca-use-package
  (elpaca-use-package-mode))
(elpaca-wait)

;;; End of elpaca installer

;; General settings
(setq
 delete-by-moving-to-trash t                      ; Use system trash on delete
 make-backup-files t                              ; Enable backups
 backup-by-copying t                              ; Copy files for backup (safer for symlinks)
 load-prefer-newer t                              ; Prefer newer versions of files
 apropos-do-all t                                 ; Show all results in apropos
 mouse-yank-at-point t                            ; Yank at point, not click location
 read-file-name-completion-ignore-case t          ; Ignore case in file name completion
 read-buffer-completion-ignore-case t             ; Ignore case in buffer name completion
 vc-follow-symlinks t                             ; Follow symlinks without confirmation

 browse-url-browser-function 'browse-url-firefox  ; Open URL links using firefox
 my-emacs-trash-dir "~/.cache/emacs/"             ; Set trash dir
 backup-directory-alist `((".*" . ,my-emacs-trash-dir))           ; Backups files
 auto-save-file-name-transforms `((".*" ,my-emacs-trash-dir t))   ; Auto-save files
 auto-save-list-file-prefix (concat my-emacs-trash-dir ".saves-") ; Auto-save crash recovery
 lock-file-name-transforms `((".*" ,my-emacs-trash-dir))          ; Lockfiles
)

(make-directory my-emacs-trash-dir t) ; Create if not exist

;; Set auto-gen conf file
(setq custom-file (locate-user-emacs-file "custom.el"))
(load custom-file :no-error-if-file-is-missing)

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

;; Mode Line
(line-number-mode 1)     ; Enable line numbers in mode line
(column-number-mode 1)   ; Enable collumn number in mode line
(size-indication-mode 1) ; Show file size in mode line

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
