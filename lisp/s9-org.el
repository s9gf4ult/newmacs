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

(define-prefix-command 'global-org-roam-map)
(define-prefix-command 'local-org-roam-map)
(set-keymap-parent local-org-roam-map global-org-roam-map)
(define-key local-org-roam-map (kbd "l") 'org-roam-node-insert)
(define-key local-org-roam-map (kbd "i") 'org-id-get-create)
(define-key local-org-roam-map (kbd "C-l") 'org-toggle-link-display)
(define-key local-org-roam-map (kbd "t t") 'org-roam-tag-add)
(define-key local-org-roam-map (kbd "t d") 'org-roam-tag-remove)
(define-key local-org-roam-map (kbd "a a") 'org-roam-alias-add)
(define-key local-org-roam-map (kbd "a d") 'org-roam-alias-remove)
(define-key local-org-roam-map (kbd "s l") 'citar-insert-citation)
(define-key local-org-roam-map (kbd "d n") 'org-roam-dailies-find-next-note)
(define-key local-org-roam-map (kbd "d p") 'org-roam-dailies-find-previous-note)


(defun s9g-org-hook ()
  (require 'org-agenda)
  (visual-line-mode 1)
  (visual-fill-column-mode 1)
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
  (local-set-key (kbd "C-M-u") 'org-up-element)
  (local-set-key (kbd "C-c r") local-org-roam-map)
  (local-set-key (kbd "C-c M-o") 'org-attach-open)
  )


(provide 's9-org)
;;; s9-org.el ends here
