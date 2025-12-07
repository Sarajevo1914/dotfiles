;;; external-pkg.el --- External packages configuration -*- lexical-binding: t; -*-

;; Themes
(use-package gruber-darker-theme)
(use-package gruvbox-theme)

(load-theme 'gruvbox-dark-soft t)

;; Undo-tree
(use-package undo-tree
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

;; Multiple-cursors
;; Read docs https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :defer t
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("C-\""        . mc/skip-to-next-like-this)
         ("C-:"         . mc/skip-to-previous-like-this)))

;; Vertico
(use-package vertico
  :demand t
  :config
  (vertico-mode)
  (setq vertico-cycle t
        vertico-resize nil
        vertico-count 15))

;; Orderless
(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))))

;; Consult
(use-package consult
  :defer t)

;; Marginalia
(use-package marginalia
  :demand t
  :config
  (marginalia-mode 1))

;; Embark
(use-package embark
  :defer t
  :bind (("C-." . embark-act)
         ("M-." . embark-dwim)
         ("C-h B" . embark-bindings))
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

;; Embark-consult
(use-package embark-consult
  :defer t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; Avy
(use-package avy
  :defer t
  :bind (("C-'" . avy-goto-char)
         ("C-c j" . avy-goto-line)))

;; Magit
(use-package magit
  :defer t)

;; Wgrep
(use-package wgrep
  :defer t
  :config
  (setq wgrep-auto-save-buffer t))

;; Which-key
(use-package which-key
  :demand t
  :config
  (which-key-mode 1))

;; Orderless
(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))))

;; Rainbow-delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Colorful-mode
(use-package colorful-mode
  :hook prog-mode
  :config
  (setq colorful-use-prefix t
        colorful-only-strings 'only-prog
        css-fontify-colors nil)
  (global-colorful-mode t))

;; Company
(use-package company
  :demand t
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
        company-backends '((company-capf company-dabbrev-code company-keywords company-dabbrev))))

;; Yasnippet
(use-package yasnippet
  :demand t
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  (setq yas-prompt-functions '(yas-dropdown-prompt yas-completing-prompt))
  (setq yas-wrap-around-region t
        yas-triggers-in-field t))

(use-package yasnippet-snippets
  :after yasnippet)

;; Vterm
(use-package vterm
  :defer t
  :bind ("C-c t" . user/vterm-here)
  :init
  (defun user/vterm-here ()
    "Open vterm in the current buffer's directory."
    (interactive)
    (let ((default-directory (or (and buffer-file-name
                                      (file-name-directory buffer-file-name))
                                 default-directory)))
      (vterm)))
  :config
  (setq vterm-always-compile-module t)  ; Auto-compile module if needed
  (setq vterm-max-scrollback 10000
        vterm-kill-buffer-on-exit t))

;; Markdown
(use-package markdown-mode
  :defer t
  :mode (("\\.md\\'" . gfm-mode)
         ("\\.markdown\\'" . markdown-mode))
  :config
  (setq markdown-command "pandoc"
        markdown-fontify-code-blocks-natively t))

;; RFC
(use-package rfc-mode
  :defer t
  :config
  (setq rfc-mode-directory (expand-file-name "~/.rfc/")))

;; xclip
(unless (display-graphic-p)
  (use-package xclip
    :ensure t
    :config
    (xclip-mode 1)))

(provide 'external-pkg)
;;; external-pkg.el ends here
