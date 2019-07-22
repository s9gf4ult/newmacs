;;; s9-org.el --- my org mode functions              -*- lexical-binding: t; -*-

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

(require 'org)

(defun s9g-org-hook ()
  (auto-fill-mode 1)
  (local-unset-key (kbd "<S-left>"))
  (local-unset-key (kbd "<S-right>"))
  (local-unset-key (kbd "<S-up>"))
  (local-unset-key (kbd "<S-down>"))
  (local-set-key (kbd "<M-S-return>") 'org-insert-todo-heading-respect-content)
  (local-set-key (kbd "<C-left>") 'org-metaleft)
  (local-set-key (kbd "<C-right>") 'org-metaright)
  (local-set-key (kbd "<C-S-left>") 'org-shiftmetaleft)
  (local-set-key (kbd "<C-S-right>") 'org-shiftmetaright)
  (local-set-key (kbd "<M-left>") 'left-word)
  (local-set-key (kbd "<M-right>") 'right-word)
  (local-set-key (kbd "<M-up>") 'org-backward-paragraph)
  (local-set-key (kbd "<M-down>") 'org-forward-paragraph)
  (local-set-key (kbd "<C-up>") 'org-metaup)
  (local-set-key (kbd "<C-down>") 'org-metadown)
  )

(provide 's9-org)
;;; s9-org.el ends here
