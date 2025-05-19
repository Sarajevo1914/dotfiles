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
(use-package orderless
  :init
  (setq completion-styles '(orderless)))
(use-package magit
  :defer t)
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . markdown-mode)
  :config
  (setq markdown-command "pandoc"))
(use-package move-text
  :config
  (move-text-default-bindings)) ; enable default keybidings (M-up / M-down)


;; theme
(use-package gruber-darker-theme :defer t)
(use-package tao-theme :defer t)
(use-package ample-theme :defer t)
(use-package gruvbox-theme :defer t)

(load-theme 'gruvbox-dark-soft t)

;; font
;;(set-frame-font "Iosevka Nerd Font 16" nil t)
(set-face-attribute 'default nil
                    :font "Iosevka Nerd Font"
                    :height 160) ;; 160 = 16pt

;; general settings
(setq inhibit-startup-message t) ; disable default splash screen
(menu-bar-mode -1) ; disable bar
(tool-bar-mode -1) ; disable toolbar
(scroll-bar-mode -1) ; disable scroll bar
(line-number-mode 1) ; show line number in minibuffer
(column-number-mode 1) ; show column number
(global-visual-line-mode 1) ; soft-wrap

;; show parent () [] {}
(show-paren-mode 1)
(setq show-paren-delay 0)
(electric-pair-mode 1)

;; line and relative numbers
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

;; whitespaces

;; global whitespaces in all buffers
(setq whitespace-style '(face trailing tabs spaces lines-tail empty indentation))
(global-whitespace-mode 1)

(defun user/delete-trailing-whitespace-on-save ()
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))

(add-hook 'prog-mode-hook #'user/delete-trailing-whitespace-on-save)
(add-hook 'text-mode-hook #'user/delete-trailing-whitespace-on-save)

;; dired
(add-hook 'dired-mode-hook 'auto-revert-mode) ; auto refresh dir when file change

;; hunspell
(setq ispell-program-name "hunspell")
(setq ispell-dictionary nil)
(setq ispell-extra-args '("-i" "utf-8" "-a"))
(defun user/list-hunspell-dictionaries ()
  "List all hunspell dictionary avalible in system."
  (interactive)
  (with-temp-buffer
    (call-process "hunspell" nil t nil "-D")
    (message "%s" (buffer-string))))

;; LSP
(use-package lsp-mode
  :hook ((prog-mode . my/conditionally-enable-lsp))
  :commands lsp
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-enable-snippet t
        lsp-prefer-capf t)

 (setq lsp-language-id-configuration
      (assq-delete-all 'emacs-lisp-mode lsp-language-id-configuration))

 (defun my/conditionally-enable-lsp ()
    "Enable `lsp` except for modes where it makes no sense."
    (unless (or (derived-mode-p 'emacs-lisp-mode)
                (derived-mode-p 'lisp-interaction-mode)
                (derived-mode-p 'lisp-mode)
                (derived-mode-p 'scheme-mode))
      (lsp))))


;; LSP UI
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-ui-doc-enable t
	lsp-ui-doc-show-with-cursor t
	lsp-ui-sideline-enable t))

;; Company
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (setq company-idle-delay 0.2
	company-minimum-prefix-length 2
	company-tooltip-align-annotations t))

;; Yasnippet
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

;; Fly
(use-package flycheck
  :hook (prog-mode . flycheck-mode))
(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
              ("C-;" . flyspell-correct-wrapper)))

;; Eldoc
(use-package eldoc
  :hook (lsp-mode . eldoc-mode))

;; Wich-key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode)
