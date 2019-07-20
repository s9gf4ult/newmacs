;;; init.el --- init                                 -*- lexical-binding: t; -*-

(setq gc-cons-threshold 1000000)

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

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
  (helm-mode 1)
  )

(use-package
  helm
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
	 ("<f7>" . helm-mini)
	 )
  )

(use-package helm-ag
  :bind (:map helm-command-map
	      ("g" . helm-do-ag)))

(use-package
 helm-rg
 :bind (:map helm-command-map
             ("r" . helm-rg)))

(use-package
 helm-rg
 :bind (:map helm-command-map
             ("r" . helm-rg)))


(use-package
  helm-projectile
  :bind (("<f8>" . helm-projectile)))

(use-package
 which-key
 :config
 (which-key-mode 1))

(use-package shakespeare-mode
  :mode "\\.hamlet\\'"
  )

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  )

(use-package
 smartparens
 :config
 (smartparens-global-mode))

(use-package haskell-mode
  :mode (("\\.cabal\\'" . cabal-mode)
	 ("\\.hs\\'" . haskell-mode)
	 )
  )

(use-package haskell-compile
  :custom
  (haskell-compile-ignore-cabal t)
  :after haskell-mode 
  )

(use-package yasnippet)

(use-package yaml-mode)

(use-package shakespeare-mode)

(use-package web-mode)

(use-package
 undo-tree
 :config
 (global-undo-tree-mode))

(use-package systemd)

(use-package string-inflection)

(use-package protobuf-mode
  :mode "\\.proto\\'"
  )

(use-package projectile)

(use-package move-text)

(use-package json-mode
  :mode "\\.json\\'"
  )

(use-package gitignore-mode)

(use-package pg-init
  :mode (("\\.v\\'" . coq-mode))
  )

(use-package nix-mode
  :mode "\\.nix\\'"
  )

(use-package crux
  :bind
  (("C-c I" . crux-find-user-init-file))
  )

(use-package
 org
 :bind
 (("C-c b" . org-switchb))
 )

(dolist (f (directory-files-recursively "~/pers/projects" "\.org$"))
	(find-file-noselect f))
