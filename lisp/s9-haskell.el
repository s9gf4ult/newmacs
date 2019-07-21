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

(use-package shakespeare-mode
  :mode "\\.hamlet\\'")

(use-package haskell-mode
  :mode (("\\.cabal\\'" . cabal-mode)
	 ("\\.hs\\'" . haskell-mode)))

(use-package haskell-compile
  :after haskell-mode
  :custom
  (haskell-compile-ignore-cabal t)
  (haskell-compile-stack-build-alt-command
   "nice -n5 stack build --bench --test --no-run-tests --no-run-benchmarks --fast --pedantic --ghc-options='-ferror-spans -j +RTS -A128m -n2m -qb0 -RTS'")
  (haskell-compile-stack-build-command
   "nice -n5 stack build --bench --test --no-run-tests --no-run-benchmarks --fast --ghc-options='-ferror-spans -instances -j +RTS -A128m -n2m -qb0 -RTS'")
  :hook ((haskell-mode . s9-haskell-hook))
  :config
  (defun s9-haskell-hook ()
    (local-set-key (kbd "<f5>") 'haskell-compile)
    (local-set-key
     (kbd "<C-f5>")
     #'(lambda ()
	 (interactive)
	 (haskell-compile -1)))
    )
   )

(provide 's9-haskell)
;;; s9-haskell.el ends here
