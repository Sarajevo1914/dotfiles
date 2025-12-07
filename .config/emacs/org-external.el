;;; org-external.el --- External packages extending org-mode -*- lexical-binding: t; -*-

;; org-modern
(use-package org-modern
  :after org
  ;; :hook ((org-mode . org-modern-mode)
  ;;        (org-agenda-finalize . org-modern-agenda))
  :config
  ;; Disable some elements for a cleaner look
  (setq org-modern-keyword nil
        org-modern-checkbox nil
        org-modern-table nil))

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

;; Custom defuns
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

(provide 'org-external)
;;; org-external.el ends here
