;;; s9-globals.el --- Global definitions             -*- lexical-binding: t; -*-

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

;;; Code:

(load "s9-defuns")

(global-set-key (kbd "C-S-k") 'crux-kill-whole-line)
(global-set-key (kbd "C-S-c") 'copy-region-as-kill)
(global-set-key (kbd "M-\\") 'delete-horizontal-and-surround-space)
(global-set-key (kbd "C-j") 'join-line)
(global-set-key (kbd "C-S-j") 'crux-top-join-line)
(global-set-key (kbd "C-;") 'comment-dwim-line)
(global-set-key (kbd "C-S-k") 'kill-line-or-region)
(global-set-key (kbd "M-d") 'smart-delete-forward)

(global-set-key (kbd "<C-tab>") #'s9g-indent-up)
(global-set-key (kbd "<C-iso-lefttab>") #'s9g-indent-down)
(global-set-key (kbd "C-c j") 'avy-goto-word-1)
(global-set-key (kbd "C-c l") 'locate-key-binding)
(global-set-key (kbd "M-*") 'pop-tag-mark)
(global-set-key (kbd "C-x \\") 'align-regexp)

(global-set-key (kbd "C-x x") 'first-error)

(global-set-key
 (kbd "C-c T")
 '(lambda ()
    (interactive)
    (insert-commented-keyword "TODO")))

(global-set-key
 (kbd "C-c F")
 '(lambda ()
    (interactive)
    (insert-commented-keyword "FIXME")))

(provide 's9-globals)
;;; s9-globals.el ends here
