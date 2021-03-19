;;; s9-tune.el --- Tune emacs even more              -*- lexical-binding: t; -*-

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

(loop
 for from across "йцукенгшщзхъфывапролджэячсмитьбюЙЦУКЕНГШЩЗХЪФЫВАПРОЛДЖ\ЭЯЧСМИТЬБЮ№"
 for to   across "qdrwbjfup;[]ashtgyneoi'zxmcvkl,.QDRWBJFUP:{}ASHTGYNEOI\"ZXMCVKL<>?"
 do
 (eval `(define-key key-translation-map (kbd ,(concat "C-" (string from))) (kbd ,(concat "C-" (string to)))))
 (eval `(define-key key-translation-map (kbd ,(concat "M-" (string from))) (kbd ,(concat "M-" (string to)))))
 (eval `(define-key key-translation-map (kbd ,(concat "C-M-" (string from))) (kbd ,(concat "C-M-" (string to))))))

(custom-set-faces
 '(default ((t (:inherit t  :height 150 :width normal :family "Hack"))))
 '(region ((t (:background "gray93")))))

(defalias 'yes-or-no-p 'y-or-n-p)

(provide 's9-tune)
;;; s9-tune.el ends here
