;;; early-init.el --- Early-init Emacs configuration -*- lexical-binding: t; -*-

;;(profiler-start 'cpu+mem)

(menu-bar-mode -1)   ; Disable menu bar
(tool-bar-mode -1)   ; Disable tool bar
(scroll-bar-mode -1) ; Disable scroll bar

;; Font
(add-to-list 'default-frame-alist '(font . "Aporetic Serif Mono-10"))

(load-theme 'wombat)

(setq
 inhibit-startup-message t   ; Disable splash screen
 initial-scratch-message nil ; Empty scratch buffer
 frame-inhibit-implied-resize t
 package-enable-at-startup nil ; Disable package.el
 gc-cons-threshold most-positive-fixnum
; gc-cons-percentage 0.6
 window-resize-pixelwise t
 frame-resize-pixelwise t
 )

;; Restore GC after boot
(add-hook 'emacs-startup-hook
          (lambda () (setq gc-cons-threshold (* 50 1024 1024)
;                           gc-cons-percentage 0.1
                           )))

;;; early-init.el ends here
