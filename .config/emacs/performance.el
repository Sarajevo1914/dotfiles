;;; performance.el --- ... -*- lexical-binding: t -*-

;; Native compilation tuning
(when (and (fboundp 'native-compile)
           (boundp 'native-comp-deferred-compilation))
  (setq native-comp-deferred-compilation t
        native-comp-jit-compilation t
        comp-async-report-warnings-errors nil)
  (when (boundp 'comp-async-jobs-number)
    (let* ((cpu-count (cond
                       ((executable-find "nproc")
                        (string-to-number
                         (string-trim (shell-command-to-string "nproc"))))
                       ((eq system-type 'darwin)
                        (string-to-number
                         (string-trim (shell-command-to-string "sysctl -n hw.ncpu"))))
                       (t 1)))
           (jobs (min cpu-count 8)))
      (setq comp-async-jobs-number (max jobs 1)))))

;; Store .eln files
(when (boundp 'native-comp-eln-load-path)
  (add-to-list 'native-comp-eln-load-path
               (expand-file-name "eln-cache/" user-emacs-directory)))

(provide 'performance)
;;; performance.el ends here
