;;; early-init.el --- Early-init Emacs configuration -*- lexical-binding: t; -*-

(profiler-start 'cpu+mem)

;; Pause GC
(setq gc-cons-threshold most-positive-fixnum)

;; Restore GC after boot
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 50 1024 1024))))
;;; early-init.el ends here
