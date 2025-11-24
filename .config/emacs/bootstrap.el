;; ;;; bootstrap.el --- Bootstrap straight.el -*- lexical-binding: t; -*-

;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el"
;;                          user-emacs-directory))
;;       (bootstrap-version 7))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;; (setq straight-use-package-by-default t)          ; use-package + straight integration
;; (setq straight-vc-git-default-clone-depth 1)     ; shallow clone
;; (setq straight-check-for-modifications
;;       '(check-on-save find-when-checking))       ; check modified packages

;; (setq straight-disable-native-compile t)

;; (straight-use-package 'use-package)
;; (require 'use-package)

;; (provide 'bootstrap)
;; ;;; bootstrap.el ends here

;;; bootstrap.el --- Bootstrap straight.el -*- lexical-binding: t; -*-

(defvar bootstrap-version)
(let* ((repo-dir (expand-file-name "straight/repos/straight.el/"
                                   user-emacs-directory))
       (bootstrap-file (expand-file-name "bootstrap.el" repo-dir)))
  ;; Clone straight.el repo if missing
  (unless (file-exists-p bootstrap-file)
    (shell-command
     (format "git clone --depth 1 https://github.com/radian-software/straight.el %s"
             repo-dir)))
  (load bootstrap-file nil 'nomessage))

(setq straight-use-package-by-default t)
(setq straight-vc-git-default-clone-depth 1)
(setq straight-check-for-modifications '(check-on-save find-when-checking))
(setq straight-disable-native-compile t)

(straight-use-package 'use-package)
(require 'use-package)

(provide 'bootstrap)
;;; bootstrap.el ends here
