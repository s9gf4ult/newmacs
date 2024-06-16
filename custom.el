(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist '((".*" concat user-emacs-directory "backups")))
 '(before-save-hook '(delete-trailing-whitespace))
 '(blink-cursor-mode nil)
 '(calendar-week-start-day 1)
 '(case-fold-search nil)
 '(column-number-mode t)
 '(confirm-kill-emacs 'yes-or-no-p)
 '(create-lockfiles nil)
 '(custom-safe-themes
   '("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default))
 '(display-buffer-alist
   '(("\\*haskell-compilation\\*" display-buffer-reuse-window
      (reusable-frames . t))
     ("magit.*" display-buffer-pop-up-window
      (reusable-frames . t))
     ("\\*NeoTree\\*" ignore
      (nil))
     (".*" display-buffer-reuse-window
      (reusable-frames . t))))
 '(display-buffer-reuse-frames t)
 '(enable-local-variables :all)
 '(fill-column 80)
 '(flyspell-auto-correct-binding [ignore])
 '(global-hl-line-mode nil)
 '(helm-ff-file-name-history-use-recentf t)
 '(indent-tabs-mode nil)
 '(initial-buffer-choice t)
 '(ispell-program-name "hunspell")
 '(kill-ring-max 600)
 '(make-backup-files nil)
 '(mouse-yank-at-point t)
 '(nix-indent-function 'nix-indent-line)
 '(recentf-max-menu-items 100)
 '(recentf-max-saved-items 10000)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 1000)
 '(scroll-margin 0)
 '(scroll-step 1)
 '(sh-basic-offset 2)
 '(standard-indent 2)
 '(tags-revert-without-query t)
 '(tool-bar-mode nil)
 '(warning-suppress-types '((comp)))
 '(woman-fill-frame t)
 '(yas-global-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(highlight ((t (:background "gainsboro")))))
