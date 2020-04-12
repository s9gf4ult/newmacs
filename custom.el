(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote ((".*" concat user-emacs-directory "backups"))))
 '(before-save-hook (quote (delete-trailing-whitespace)))
 '(blink-cursor-mode nil)
 '(case-fold-search nil)
 '(column-number-mode t)
 '(confirm-kill-emacs (quote yes-or-no-p))
 '(create-lockfiles nil)
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
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
 '(helm-ff-file-name-history-use-recentf t)
 '(indent-tabs-mode nil)
 '(initial-buffer-choice t)
 '(ispell-program-name "hunspell")
 '(kill-ring-max 600)
 '(make-backup-files nil)
 '(mouse-yank-at-point t)
 '(nix-indent-function (quote nix-indent-line))
 '(recentf-max-menu-items 100)
 '(recentf-max-saved-items 10000)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1000)
 '(scroll-margin 0)
 '(scroll-step 1)
 '(sh-basic-offset 2)
 '(standard-indent 2)
 '(tool-bar-mode nil)
 '(yas-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight ((t (:background "gainsboro")))))
