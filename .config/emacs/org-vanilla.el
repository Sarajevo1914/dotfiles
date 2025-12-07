;;; org-vanilla.el --- Minimal vanilla org-mode configuration -*- lexical-binding: t; -*-

(require 'org)

;; Basic org settings
(setq org-startup-indented nil
      org-pretty-entities t
      org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-image-actual-width '(300)
      org-log-done 'time
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-confirm-babel-evaluate nil
      org-edit-src-content-indentation 0)

;; Enable babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (shell . t)))

;; Keybindings
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)

(provide 'org-vanilla)
;;; org-vanilla.el ends here
