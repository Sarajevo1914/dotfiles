;;; org-external.el --- External packages extending org-mode -*- lexical-binding: t; -*-

;; org-modern: improved org-mode visuals
(use-package org-modern
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :config
  ;; Disable some elements for a cleaner look
  (setq org-modern-keyword nil
        org-modern-checkbox nil
        org-modern-table nil))

;; org-auto-tangle: automatically tangle files only when enabled via header
(use-package org-auto-tangle
  :after org
  :hook (org-mode . org-auto-tangle-mode)
  :config
  ;; Only tangle files with #+auto_tangle: t header
  (setq org-auto-tangle-default nil))

(provide 'org-external)
;;; org-external.el ends here
