(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(before-save-hook (quote (delete-trailing-whitespace)))
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(display-buffer-alist
   (quote
    (("\\*haskell-compilation\\*" display-buffer-reuse-window
      (reusable-frames . t))
     ("magit.*" display-buffer-pop-up-window
      (reusable-frames . t))
     ("\\*NeoTree\\*" ignore
      (nil))
     (".*" display-buffer-reuse-window
      (reusable-frames . t)))))
 '(display-buffer-reuse-frames t)
 '(flyspell-auto-correct-binding [ignore])
 '(global-hl-line-mode nil)
 '(indent-tabs-mode nil)
 '(initial-buffer-choice t)
 '(ispell-program-name "hunspell")
 '(kill-ring-max 600)
 '(make-backup-files nil)
 '(mouse-yank-at-point t)
 '(nix-indent-function (quote nix-indent-line))
 '(recentf-max-saved-items 1000)
 '(scroll-bar-mode nil)
 '(sh-basic-offset 2)
 '(standard-indent 2)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#d4d4d4" :foreground "#2c2c2c" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "PfEd" :family "Liberation Mono"))))
 '(highlight ((t (:background "gainsboro")))))
