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

(use-package undo-tree
 :config
 (global-undo-tree-mode))

(use-package neotree
  :bind (("<f12>" . neotree-toggle))
  :custom
  (neo-create-file-auto-open t)
  (neo-smart-open t)
 )

;; For snippets and stuff
(use-package s)

(use-package windmove
  :bind (("<S-up>" . windmove-up)
	 ("<S-down>" . windmove-down)
	 )
  )

(use-package ace-window
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

(use-package narrow-indirect
  :bind
  ("C-x 4 n n" . ni-narrow-to-region-indirect-other-window)
  )

(use-package magit
 :bind (("C-x g" . magit))
 :custom
 (magit-commit-show-diff nil)
 (magit-diff-arguments
   (quote
    ("--stat" "--no-ext-diff" "--diff-algorithm=histogram")))
 (magit-log-margin (quote (t "%Y-%m-%d %H:%M " magit-log-margin-width t 18)))
 :hook (magit-mode . turn-on-magit-gitflow)
 :config
 (magit-auto-revert-mode 1)
 (require 'magit-gitflow)
 )

(use-package smartparens
  :demand t
  :bind (("<C-right>" . sp-forward-slurp-sexp)
	 ("<C-left>" . sp-forward-barf-sexp)
	 ("<M-down>" . sp-splice-sexp-killing-forward)
	 ("<M-up>" . sp-splice-sexp-killing-backward)
	 )
  :config
  (show-smartparens-global-mode))

(use-package diff-hl
  :config
  (require 'flycheck)
  (global-diff-hl-mode)
  :custom-face
  (diff-hl-change ((t (:background "deep sky blue" :foreground "blue3"))))
  (diff-hl-delete ((t (:inherit diff-removed :background "firebrick" :foreground "red3"))))
  (diff-hl-insert ((t (:inherit diff-added :background "sea green" :foreground "green4"))))
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :demand t
  )

(use-package which-key
  :config
  (which-key-mode 1))

(use-package move-text)

(use-package crux
  :demand t
  :bind
  (("C-c I" . crux-find-user-init-file)
   ("C-c C" . crux-find-user-custom-file)
   ))

(use-package yasnippet
  :custom ((yas-global-mode t)))

;;;;;;;;;;;
;; helm  ;;
;;;;;;;;;;;

(use-package helm
  :demand t
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
	 ("<f7>" . helm-mini)
	 ("M-." . helm-etags-select)
	 )
  :custom
  ((helm-etags-execute-action-at-once-if-one nil)
   (helm-buffer-details-flag nil)
   ))


(use-package helm-config
  :config
  (helm-mode 1))

(use-package helm-ag
  :demand t
  :bind (:map helm-command-map
	      ("g" . helm-do-ag)))

(use-package helm-rg
  :custom-face
  (helm-rg-file-match-face ((t (:foreground "dark blue" :underline t))))
  )

(use-package projectile
  :config
  (projectile-global-mode))

(use-package helm-projectile
  :bind (("<f8>" . helm-projectile)
	 :map helm-command-map
         ("r" . helm-projectile-rg)))

;;;;;;;;;;;;;
;; haskell ;;
;;;;;;;;;;;;;


(use-package haskell-snippets
  :after haskell-mode
  :config
  (yas-reload-all)
  )

(use-package shakespeare-mode
  :mode (("\\.hamlet\\'" . shakespeare-hamlet-mode)))

(use-package s9-haskell
  :mode (("\\.cabal\\'" . haskell-cabal-mode)
	 ("\\.hs\\'" . haskell-mode))
  :hook ((haskell-mode . s9g-haskell-mode-hook)
	 (haskell-cabal-mode . s9g-cabal-mode-hook)
	 )
  :custom
  (haskell-compile-stack-build-alt-command
   "nice -n5 stack build --bench --test --no-run-tests --no-run-benchmarks --fast --pedantic --ghc-options='-ferror-spans -j12 +RTS -A128m -n2m -qb0 -RTS'")
  (haskell-compile-stack-build-command
    "nice -n5 stack build --bench --test --no-run-tests --no-run-benchmarks --fast --ghc-options='-ferror-spans -instances -j12 +RTS -A128m -n2m -qb0 -RTS'")
  (haskell-process-args-stack-ghci (quote ("--ghci-options" "-ferror-spans")))
  (haskell-compile-ignore-cabal t)
  (haskell-stylish-on-save nil)
  (haskell-compiler-type 'stack)
  (haskell-process-type 'stack-ghci)
  )

;;;;;;;;;;;
;; theme ;;
;;;;;;;;;;;

(use-package solarized-theme
  :custom
  ((solarized-height-minus-1 1.0)
   (solarized-height-plus-1 1.0)
   (solarized-height-plus-2 1.0)
   (solarized-height-plus-3 1.0)
   (solarized-height-plus-4 1.0)
   (solarized-scale-org-headlines nil)
   (solarized-use-variable-pitch nil)
   )
  )

;;;;;;;;;;;;;;;;;
;; other modes ;;
;;;;;;;;;;;;;;;;;

(use-package mustache-mode
  :mode ("\\.mustache\\'"))

(use-package yaml-mode
  :mode ("\\.yaml\\'" "\\.yml\\'")
  ;; :hook
  ;; ((yaml-mode
  ;;   . (lambda ()
  ;;       (outline-minor-mode)
  ;;       (define-key yaml-mode-map (kbd "M-TAB") 'outline-toggle-children)
  ;;       (setq outline-regexp "^ *\\([A-Za-z0-9_-]*: *[>|]?$\\|-\\b\\)"))))
  )

(use-package web-mode
  :mode ("\\.html\\'" "\\.php\\'")
  :custom-face
  (web-mode-html-tag-face ((t (:foreground "blue"))))
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-code-indent-offset 2)
  )

(use-package rust-mode
  :bind
  (:map rust-mode-map
        ("<f5>" . (lambda () (interactive)
                    (save-some-buffers t)
                    (rust-compile))))
  :hook
  (rust-mode . smartparens-mode)
  )

(use-package systemd
  :mode "\\.service\\'")

(use-package string-inflection)

(use-package protobuf-mode
  :mode "\\.proto\\'"
  )

(use-package json-mode
  :mode "\\.json\\'"
  :custom ((js-indent-level 2))
  )

(use-package gitignore-mode
  :mode "\\.gitignore\\'"
  )

(use-package proof-site ; proof-general
  :mode (("\\.v\\'" . coq-mode))
  :custom-face
  (proof-locked-face
   ((t
     (:background "gray85" :extend t))))
  :demand t
  :hook
  (coq-mode
   .
   (lambda (interactive)
     (smartparens-mode 1))))

(use-package nix-mode
  :mode "\\.nix\\'"
  :hook
  (nix-mode . (lambda () (interactive)
                (when (< (buffer-size) 500000) ; sm is slow on huge nix buffers
                  (smartparens-mode 1)))))

(use-package elisp-mode
  :hook
  (emacs-lisp-mode
   . (lambda () (interactive)
       (smartparens-mode 1))))

(use-package es-mode
  :mode "\\.es\\'"
  )

(use-package dockerfile-mode
  :mode "Dockerfile"
  )

(use-package markdown-mode
  :custom
  ((markdown-code-lang-modes
    '(("ocaml" . tuareg-mode)
      ("elisp" . emacs-lisp-mode)
      ("ditaa" . artist-mode)
      ("asymptote" . asy-mode)
      ("dot" . fundamental-mode)
      ("sqlite" . sql-mode)
      ("calc" . fundamental-mode)
      ("C" . c-mode)
      ("cpp" . c++-mode)
      ("C++" . c++-mode)
      ("screen" . shell-script-mode)
      ("shell" . sh-mode)
      ("bash" . sh-mode)
      ("xml" . web-mode)))
   (markdown-asymmetric-header t)
   )
  :bind
  (:map markdown-mode-map
        ("№" . "#")
        ("<C-right>" . markdown-demote)
        ("<C-left>" . markdown-promote)
        ("<S-return>" . markdown-insert-header-dwim)
        ("C-c q" . markdown-insert-gfm-code-block)
        )
  )

(use-package lua-mode
  :hook
  (lua-mode
   . (lambda () (interactive)
       (smartparens-mode t)
       )))

(use-package neuron-mode
  :ensure t
  :custom
  ((neuron-default-zettelkasten-directory "~/pers/neuron"))
  :bind
  (("C-c z z" . neuron-new-zettel)
   ("C-c z e" . neuron-edit-zettel)
   :map neuron-mode-map
   ("C-c C-v" . neuron-create-and-insert-zettel-link)
   ("<f5>" . (lambda () (interactive)
               (save-some-buffers t)
               (neuron-refresh)))
   ("<C-return>" . neuron-follow-thing-at-point)
   ("<return>" . markdown-enter-key)
   ))

(require 'org-agenda)

(use-package s9-org
  :bind (("C-c b" . org-switchb)
	 ("C-c c" . org-capture)
	 :map org-mode-map
	 ("<S-left>" . nil)
	 ("<S-right>" . nil)
	 ("<S-up>" . nil)
	 ("<S-down>" . nil)
         ("C-c C-x i" . org-set-custom-id)
         ("C-c C-x l" . org-word-to-custom-link)
         ("C-c o" . org-open-at-point)
         :map org-agenda-mode-map
	 ("<S-left>" . nil)
	 ("<S-right>" . nil)
	 ("<S-up>" . nil)
	 ("<S-down>" . nil)
         )
  :custom ((org-todo-keywords
            '((sequence "TODO(t)" "HOLD(h@)" "INPROGRESS(i!)" "|" "DONE(d!)" "CANCELLED(c@)")
              (sequence "PROBLEM(p)" "SOLVED(s@)")
              (sequence "QUESTION(Q)" "|" "YES(y@)" "NO(Y@) " "UNKNOWN(u@)")
              (s equence "BUY(b)" "|" "REFUSED(r@)" "GOT(g)")
              ))
           (org-clock-persist t)
           (org-capture-templates
	    (quote
	     (("t" "Todo prefix")
	      ("tt" "Todo sometime" entry
	       (file+headline "~/pers/capture.org" "Tasks")
	       (file "~/.emacs.d/org-templates/todo")
	       :prepend t)
	      ("tr" "Travel check list" entry
	       (file+headline "~/pers/capture.org " "Tasks ")
	       (file "~/.emacs.d/org-templates/travel")
	       :prepend t)
	      ("ts" "Scheduled todo item" entry
	       (file+headline "~/pers/capture.org" "Tasks")
	       (file "~/.emacs.d/org-templates/scheduled")
	       :prepend t)
	      ("tc" "Code linked todo" entry
	       (file+headline "~/pers/capture.org" "Tasks")
	       (file "~/.emacs.d/org-templates/code")
	       :prepend t)
	      ("tu" "Url assigned todo" entry
	       (file+headline "~/pers/capture.org" "Tasks")
	       (file "~/.emacs.d/org-templates/urltodo")
	       :prepend t)
	      ("n" "Note" entry
	       (file+headline "~/pers/projects/life/info/notes.org" "Notes")
	       (file "~/.emacs.d/org-templates/note")
	       :prepend t)
	      ("l" "Log entry")
	      ("ll" "Simple log entry" entry
	       (file+olp+datetree "~/pers/projects/life/log/log.org")
	       "* %?")
	      ("lh" "Health log" entry
	       (file+olp+datetree "~/pers/projects/life/log/health.org")
	       "* %?")
	      ("lm" "Mother log" entry
	       (file+olp+datetree "~/pers/projects/life/log/mother.org")
	       "* %? ")
	      ("u" "Url" entry
	       (file+headline "~/pers/urls.org" "Urls")
	       (file "~/.emacs.d/org-templates/url")
	       :prepend t))))
	   (org-default-notes-file "~/pers/refile.org")
	   (org-extend-today-until 5)
	   (org-log-done (quote time))
	   (org-refile-targets
	    (quote
             (("~/pers/projects/typeableio/typeableio.org" :level . 1)
               ("~/pers/projects/life/todo/read.org" :level . 1)
               ("~/pers/todo.org" :maxlevel . 2))
	     ))
           (org-agenda-files
            '("~/pers/todo.org" "~/pers/projects/typeableio/typeableio.org"))
           (org-priority-default (string-to-char "F"))
           (org-priority-lowest (string-to-char "F"))
	   (org-reverse-note-order t))
  :custom-face
  (org-todo ((t (:weight bold :foreground "#CF4646"))))
  :hook ((org-mode . s9g-org-hook))
  :demand t
  )

(use-package tramp
  :custom ((tramp-default-method "ssh" nil (tramp))
	   (tramp-encoding-shell "/run/current-system/sw/bin/zsh" nil (tramp)))
  :config
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

(use-package company :demand t)

(use-package smooth-scroll
  :demand 1
  :config
  (smooth-scroll-mode 1))


(provide 's9-packages)
;;; s9-packages.el ends here

(require 'zoom-frm)
