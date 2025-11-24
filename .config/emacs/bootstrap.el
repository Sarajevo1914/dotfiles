;;; bootstrap.el --- Bootstrap straight.el -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'json))

(defvar bootstrap-version)
(let* ((repo-dir (expand-file-name "straight/repos/straight.el/"
                                   user-emacs-directory))
       (bootstrap-file (expand-file-name "bootstrap.el" repo-dir)))
  (unless (file-exists-p bootstrap-file)
    (shell-command
     (format "git clone --depth 1 https://github.com/radian-software/straight.el %s"
             repo-dir)))
  (load bootstrap-file nil 'nomessage))

(require 'json)

(setq straight-use-package-by-default t)
(setq straight-vc-git-default-clone-depth 1)
(setq straight-check-for-modifications '(check-on-save find-when-checking))
(setq straight-disable-native-compile t)

(straight-use-package 'use-package)
(require 'use-package)

;; Load Org IMMEDIATELY to prevent version mismatch
(straight-use-package 'org)

(provide 'bootstrap)
;;; bootstrap.el ends here
