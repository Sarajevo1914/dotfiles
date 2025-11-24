;;; init.el --- Main initialization file -*- lexical-binding: t; -*-

;; Load bootstrap (pkg manager)
(load (expand-file-name "bootstrap.el" user-emacs-directory))

;; Load performance tuning
(load (expand-file-name "performance.el" user-emacs-directory))

;; Load vanilla settings (only built-in pkg and conf)
(load (expand-file-name "vanilla.el" user-emacs-directory))

;; Load external packages
(load (expand-file-name "external-pkg.el" user-emacs-directory))

;; Load vanilla org config
(load (expand-file-name "org-vanilla.el" user-emacs-directory))

;; Load extrenal org pkg config
(load (expand-file-name "org-external.el" user-emacs-directory))

;; Load extra config
(let ((modules-dir (expand-file-name "extra-config" user-emacs-directory)))
  (when (file-directory-p modules-dir)
    (dolist (file (directory-files modules-dir t "\\.el$"))
      (condition-case err
          (load file)
        (error (message "Error loading %s: %s" file err))))))

;;; init.el ends here
