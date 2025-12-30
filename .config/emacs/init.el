;;; init.el --- Vanilla Emacs configuration -*- lexical-binding: t; -*-

(load (expand-file-name "performance.el" user-emacs-directory) nil t)

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

;; Update org
(elpaca org)
(elpaca-wait)

;; use-package integration
(elpaca elpaca-use-package
  (elpaca-use-package-mode))
(elpaca-wait)

;;; End of elpaca installer

;;; Use-package
(setq
 use-package-always-defer t
 use-package-always-ensure t
 use-package-expand-minimally t
 )

;;; General and internals settings
(setq
 delete-by-moving-to-trash t                                      ; Use system trash on delete
 make-backup-files t                                              ; Enable backups
 backup-by-copying t                                              ; Copy files for backup (safer for symlinks)
 load-prefer-newer t                                              ; Prefer newer versions of files
 apropos-do-all t                                                 ; Show all results in apropos
 mouse-yank-at-point t                                            ; Yank at point, not click location
 read-file-name-completion-ignore-case t                          ; Ignore case in file name completion
 read-buffer-completion-ignore-case t                             ; Ignore case in buffer name completion
 vc-follow-symlinks t                                             ; Follow symlinks without confirmation
 browse-url-browser-function 'browse-url-firefox                  ; Open URL links using firefox
 my-emacs-trash-dir "~/.cache/emacs/"                             ; Set trash dir
 backup-directory-alist `((".*" . ,my-emacs-trash-dir))           ; Backups files
 auto-save-file-name-transforms `((".*" ,my-emacs-trash-dir t))   ; Auto-save files
 auto-save-default t                                              ; Enable auto-save
 auto-save-interval 100                                           ; Save every n events
 auto-save-timeout 15                                             ; Auto-save every n seconds
 version-control t                                                ; Enable version-control
 kept-old-versions 5                                              ; Keep n old versions
 kept-new-versions 10                                             ; Keep n new versions
 delete-old-versions t                                            ; Delete old versions
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
(line-number-mode 1)             ; Enable line numbers in mode line
(column-number-mode 1)           ; Enable collumn number in mode line
(size-indication-mode 1)         ; Show file size in mode line

;; Behavior
(global-visual-line-mode 1)  ; Soft-wrap lines
(global-auto-revert-mode 1)  ; Auto-revert changed files
(savehist-mode 1)            ; Save minibuffer history
(recentf-mode 1)             ; Recent files mode
(delete-selection-mode 1)    ; Replace selection when typing

;; Parentheses and delimiters
(setq
 show-paren-delay 0
 show-paren-style 'mixed
 )

(show-paren-mode 1)          ; Highlight matching parentheses
(electric-pair-mode 1)       ; Automatically insert closing delimiters

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

;; Whitespace TODO

;; Delete trailing whitespace on save
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Which-key
(use-package which-key
  :demand t
  :config
  (which-key-mode 1))

;;; EXTERNAL PKGs

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
  :demand t
  :config
  (marginalia-mode 1))

;; Consult
(use-package consult)

;; Embark
(use-package embark
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings))
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

;; Embark-consult
(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Avy
(use-package avy
  :bind (("C-'" . avy-goto-char)
         ("C-c j" . avy-goto-line)))

;; Orderless
(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))))

;; Transient for magit
(use-package transient)

;; Magit
(use-package magit)

;; Wgrep
(use-package wgrep
  :config
  (setq wgrep-auto-save-buffer t))

;; Markdown
(use-package markdown-mode
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "pandoc"
        markdown-fontify-code-blocks-natively t))

;; RFC
(use-package rfc-mode
  :config
  (setq rfc-mode-directory (expand-file-name "~/.rfc/")))

;; xclip
(unless (display-graphic-p)
  (use-package xclip
    :hook (after-init . xclip-mode)))

;; Multiple-cursors
;; Read docs https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("C-\""        . mc/skip-to-next-like-this)
         ("C-:"         . mc/skip-to-previous-like-this)))

;; Rainbow-delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Colorful-mode
(use-package colorful-mode
  :hook (prog-mode . colorful-mode)
  :config
  (setq colorful-use-prefix t
        colorful-only-strings t
        css-fontify-colors nil))

;; Undo-tree
(use-package undo-tree
  :demand t
  :config
  (setq undo-tree-history-directory-alist
        `(("." . ,my-emacs-trash-dir))
        undo-tree-auto-save-history t)
  (global-undo-tree-mode))

;; Move-text
(use-package move-text
  :demand t
  :config
  (move-text-default-bindings)
  :bind (("M-p" . move-text-up)
         ("M-n" . move-text-down)))

;;; ORG

;; org vanilla
(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture))
  :config
  (setq
   org-startup-indented nil
   org-pretty-entities t
   org-hide-emphasis-markers t
   org-startup-with-inline-images t
   org-image-actual-width '(300)
   org-log-done 'time
   org-src-fontify-natively t
   org-src-tab-acts-natively t
   org-confirm-babel-evaluate nil
   org-edit-src-content-indentation 0
   )

  ;; Babel - SOLO cuando usas Org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t))))

;; org-modern
(use-package org-modern
  :after org
  ;; :hook ((org-mode . org-modern-mode)
  ;;        (org-agenda-finalize . org-modern-agenda))
  :config
  ;; Disable some elements for a cleaner look
  (setq
   org-modern-keyword nil
   org-modern-checkbox nil
   org-modern-table nil
   ))

;; org-auto-tangle
(use-package org-auto-tangle
  :after org
  :config
  ;; Only tangle files with #+auto_tangle: t header
  (setq org-auto-tangle-default nil))

;; org-roam
(use-package org-roam
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory (file-truename "~/roam"))
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)   ;; Toggle backlinks buffer
         ("C-c n f" . org-roam-node-find)       ;; Search/create a note
         ("C-c n i" . org-roam-node-insert)     ;; Create link to a note (creates it if not exists)
         ("C-c n c" . org-roam-capture)         ;; Quick capture
         ("C-c n e" . org-roam-extract-subtree) ;; Extract subtree into new note
         )
  :config
  (org-roam-db-autosync-mode 1))

;; Custom defuns for ORG
(defun user/org-roam-convert-file (file)
  "Convert FILE into an org-roam note by adding missing title, heading, and ID."
  (with-current-buffer (find-file-noselect file)
    ;; Add #+title:
    (goto-char (point-min))
    (unless (re-search-forward "^#\\+title:" nil t)
      (goto-char (point-min))
      (insert "#+title: " (file-name-base file) "\n\n"))

    ;; Ensure heading
    (goto-char (point-min))
    (unless (re-search-forward "^\\* " nil t)
      (goto-char (point-min))
      (insert "* " (file-name-base file) "\n"))

    ;; Add ID inside heading
    (goto-char (point-min))
    (re-search-forward "^\\* " nil t)
    (org-id-get-create)

    (save-buffer)
    (kill-buffer)))

(defun user/org-roam-convert-directory (dir)
  "Convert all .org files in DIR into org-roam nodes.
Skips .git/, archive files, and files that already have an ID."
  (interactive "DDirectory: ")
  (dolist (file (directory-files-recursively dir "\\.org$"))
    (unless (or
             (string-match-p "\\.git/" file)
             (string-match-p "archive\\.org$" file)
             (string-match-p "README\\.org$" file)
             (with-temp-buffer
               (insert-file-contents file)
               (goto-char (point-min))
               (re-search-forward "^:ID:" nil t)))
      (user/org-roam-convert-file file)))
  (message "Finished converting notes to org-roam format."))

;; Load time
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (message "Emacs loaded in %s" (emacs-init-time))
;;             (when (profiler-running-p)
;;               (profiler-stop)
;;               (profiler-report))))
;;; init.el ends here
