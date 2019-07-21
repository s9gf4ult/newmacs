;;; s9-packages.el --- load all my packages          -*- lexical-binding: t; -*-

;; Copyright (C) 2019

;; Author:  <razor@gazoline>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:



;;;;;;;;;;;;;;;;;;
;; convenience  ;;
;;;;;;;;;;;;;;;;;;

(use-package
 neotree
 :bind (("<f12>" . neotree-toggle))
 )

(use-package
  windmove
  :bind (("<S-up>" . windmove-up)
	 ("<S-down>" . windmove-down)
	 )
  )

(use-package
  ace-window
  :demand t ; defering does not work
  :bind (("<S-left>" . aw-switch-prev-window)
	 ("<S-right>" . aw-switch-next-window)
	 )
  :config
  (require 'cl)
  (dotimes (i 9)
    (lexical-let* ((idx i) ; to bind it lexically
		   (keyn (+ 1 idx))
                   (key (format "C-%d" keyn)))
    (bind-key
     key
     (lambda ()
       (interactive)
       (lexical-let*
	   ((wlist (aw-window-list))
            (w (or
                (nth idx wlist)
                (car (last wlist)))))
         (unless (null w)
           (aw-switch-to-window w))))))))

(use-package
 magit
 :bind (("C-x g" . magit))
 :custom
 (magit-commit-show-diff nil)
 (magit-diff-arguments
   (quote
    ("--stat" "--no-ext-diff" "--diff-algorithm=histogram")))
 (magit-log-margin (quote (t "%Y-%m-%d %H:%M " magit-log-margin-width t 18)))
 )

(use-package smartparens
  :demand t
  :bind (("<C-right>" . sp-forward-slurp-sexp)
	 ("<C-left>" . sp-forward-barf-sexp)
	 ("<M-down>" . sp-splice-sexp-killing-forward)
	 ("<M-up>" . sp-splice-sexp-killing-backward)
	 )
  :config
  (smartparens-global-mode)
  (show-smartparens-global-mode))

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  :custom-face
  (diff-hl-change ((t (:background "deep sky blue" :foreground "blue3"))))
  (diff-hl-delete ((t (:inherit diff-removed :background "firebrick" :foreground "red3"))))
  (diff-hl-insert ((t (:inherit diff-added :background "sea green" :foreground "green4")))))

(use-package which-key
  :config
  (which-key-mode 1))

;;;;;;;;;;;
;; helm  ;;
;;;;;;;;;;;

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
	 ("<f7>" . helm-mini)
	 ))

(use-package helm-config
  :config
  (helm-mode 1)
  :after helm)

(use-package helm-ag
  :bind (:map helm-command-map
	      ("g" . helm-do-ag))
  :after helm)

(use-package helm-rg
  :bind (:map helm-command-map
              ("r" . helm-rg))
  :after helm)

(use-package helm-projectile
  :bind (("<f8>" . helm-projectile)))


(use-package shakespeare-mode
  :mode "\\.hamlet\\'"
  )

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

(use-package gitignore-mode
  :mode "\\.gitignore\\'"
  )

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


(provide 's9-packages)
;;; s9-packages.el ends here
