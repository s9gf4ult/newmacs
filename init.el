;;; init.el --- init                                 -*- lexical-binding: t; -*-

(setq gc-cons-threshold 1000000)

(run-with-idle-timer 10 t 'garbage-collect)

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))
(setq custom-file (concat user-emacs-directory "custom.el"))

(require 'package)
(package-initialize 'noactivate)
(eval-when-compile
  (require 'use-package))

(add-to-list 'load-path (concat user-emacs-directory "lisp"))

(load "narrow-indirect")
(load "s9-packages")
(load "s9-globals")
(load "s9-tune")

(load custom-file)

(dolist (f
         (cons "~/pers/todo.org"
               (directory-files-recursively "~/pers/projects" "\.org$")))
	(find-file-noselect f))
(put 'narrow-to-region 'disabled nil)
