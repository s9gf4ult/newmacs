;;; s9-haskell.el --- haskell config                 -*- lexical-binding: t; -*-

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

(require 'haskell-mode)
(require 'haskell-compile)
(require 'haskell-navigate-imports)
(require 'neotree)
(require 'yasnippet)
(require 'haskell-snippets)

(defun haskell-end-of-line-and-indent (arg)
  (interactive "p")
  (end-of-line)
  (let ((lines (or arg
                   1)))
    (dotimes (none lines)
      (haskell-indentation-newline-and-indent))))

(defun s9g-haskell-set-buffer-name ()
  (let ((modname (haskell-guess-module-name)))
    (unless (string-equal "" modname)
      (rename-buffer modname t))))

(defcustom s9g-haskell-compile-cabal-build-command
  "nice -n5 stack build / --bench --test --no-run-tests --no-run-benchmarks --fast --ghc-options='-ferror-spans -j +RTS -A128m -n2m -qb0 -RTS'"
  "Compile all cabal command"
  :type 'string
  )

(defcustom s9g-haskell-compile-cabal-build-alt-command
  "nice -n5 stack build / --bench --test --no-run-tests --no-run-benchmarks --fast --pedantic --ghc-options='-ferror-spans -j +RTS -A128m -n2m -qb0 -RTS'"
  "Compile all cabal command"
  :type 'string
  )


(defun s9g-haskell-compile-all (&optional alt)
  (interactive "P")
  (let ((haskell-compile-cabal-build-command s9g-haskell-compile-cabal-build-command)
        (haskell-compile-cabal-build-alt-command s9g-haskell-compile-cabal-build-alt-command))
    (s9g-haskell-compile alt)))

(defun s9g-haskell-compile (&optional alt)
  (interactive "P")
  (save-some-buffers t)
  (if alt
      (haskell-compile '-)
    (haskell-compile)))

(defun haskell-neotree-toggle-proj ()
  (interactive)
  (if (neo-global--window-exists-p)
      (neotree-hide)
    (haskell-neotree-show)))

;;;###autoload
(defun haskell-neotree-show ()
  "Show the NeoTree window."
  (interactive)
  (let ((cw (selected-window))
        (path (buffer-file-name)))  ;; save current window and buffer
    (neotree-dir (haskell-cabal-find-dir))
    (neotree-find path)
    (neo-global--select-window)
    (when neo-toggle-window-keep-p
      (select-window cw))))

(defun s9g-haskell-yesod-handler-name ()
  (interactive)
  (let* ((p1 (line-beginning-position))
         (p2 (line-end-position))
         (lval (buffer-substring-no-properties p1 p2))
         (w (cdr (split-string lval)))  ; split to words and drop routes
         (rname (car w))
         (methods (cdr w)))
    (if (> (length methods) 0)
        (progn
          (kill-whole-line)
          (previous-line)
          (loop for m in methods do
                (let* ((name (concat (downcase m) rname))
		       (l1 (concat name " :: Handler TypedContent"))
		       (l2 (concat name " = error \"" name " not implemented\"")))
                  (end-of-line)
                  (newline)
                  (insert l1) (newline)
                  (insert l2) (newline)))))))


(defun s9g-cabal-mode-hook ()
  (local-set-key (kbd "<f5>") 's9g-haskell-compile)
  (local-set-key (kbd "<f6>") 's9g-haskell-compile-all)
  (local-set-key (kbd "<f12>") 'haskell-neotree-toggle-proj))

(defun hemmet-expand-region ()
  (interactive)
  (let ((f (lambda (b e)
             (shell-command-on-region
	      b e "hemmet" t t "*hemmet error*" t))))
    (if (region-active-p)
        (funcall f (region-beginning) (region-end))
      (funcall f (line-beginning-position) (line-end-position)))
    ))

(defun s9g-haskell-mode-hook ()
  (yas-minor-mode 1)
  (local-set-key (kbd "<f5>") 's9g-haskell-compile)
  (local-set-key (kbd "<f6>") 's9g-haskell-compile-all)
  (local-set-key (kbd "<f12>") 'haskell-neotree-toggle-proj)
  (local-set-key
   (kbd "<f9>")
   #'(lambda ()
       (interactive)
       (haskell-cabal-visit-file t)))
  (local-set-key (kbd "C-c s") 'haskell-sort-imports)
  (local-set-key (kbd "M-p") 'haskell-navigate-imports)
  (local-set-key (kbd "<S-return>") 'haskell-end-of-line-and-indent)
  (local-set-key (kbd "<M-S-up>") 'move-text-up)
  (local-set-key (kbd "<M-S-down>") 'move-text-down)
  (local-set-key (kbd "C-c C-j") 'hemmet-expand-region)
  (haskell-indentation-mode +1)
  (s9g-haskell-set-buffer-name)
  (smartparens-mode 1)
  (sp-pair "'" nil :actions :rem)
  (add-to-list 'write-file-functions 'delete-trailing-whitespace)
  )

(provide 's9-haskell)
;;; s9-haskell.el ends here
