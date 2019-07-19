;;; init.el --- init                                 -*- lexical-binding: t; -*-

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))

(require 'package)
(package-initialize 'noactivate)
(eval-when-compile
  (require 'use-package))

(use-package
 neotree
 :bind (("<f12>" . neotree-toggle))
 )

(use-package
 magit
 :bind (("C-x g" . magit))
 )

(use-package
 helm-config
 :config
 (helm-mode 1))

(use-package
  helm
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files))
  )

(use-package
 helm-ag
 :bind (("C-c h x" . helm-do-ag-project-root)
        ("C-c h g" . helm-do-ag)
        )
 :after helm
 )

(use-package
 which-key
 :config
 (which-key-mode 1))

(use-package
 smartparens
 :config
 (smartparens-global-mode))

(use-package haskell-mode)

(use-package yasnippet)
