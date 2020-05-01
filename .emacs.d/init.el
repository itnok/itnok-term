;;; -*- mode: emacs-lisp; -*-
;;; -*- lexical-binding: nil -*-

;;; MELPA
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)

;;; defaults
(setq-default make-backup-files nil)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)
(setq default-major-mode 'text-mode)
(column-number-mode t)
(electric-indent-mode -1)

;;; dracula theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)

;;; treat .cu files as c-mode
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c-mode))

;;; configs
(require 'cc-mode)
(defconst config-default-c-style-name "builtin")
(defconst config-default-c-style-def
  `("gnu"
    (indent-tabs-mode . nil)
    (c-basic-offset . 2)
    (c-offsets-alist .
                     ((substatement-open . 0)
                      (access-label . /)
                      (case-label . +)))))
(defun config-default-load ()
  (c-add-style config-default-c-style-name config-default-c-style-def)

  ;;; c-mode-common ({c, c++, objc, java, idl, pike, awk}-mode-hook)
  (add-hook 'c-mode-common-hook (lambda ()
    (c-set-style config-default-c-style-name)))
  ;;; sh-mode
  (add-hook 'sh-mode-hook (lambda ()
    (setq sh-basic-offset 2)))
  ;;; nxml-mode
  (add-hook 'nxml-mode-hook (lambda ()
    (setq nxml-child-indent 2)))
  ;;; vhdl-mode
  (add-hook 'vhdl-mode-hook (lambda ()
    ;(vhdl-set-style "IEEE")
    (setq vhdl-basic-offset 4)
    (setq vhdl-child-indent 4)
    (setq vhdl-electric-mode nil)))
  ;;; python-mode
  (add-hook 'python-mode-hook (lambda ()
    (setq python-indent 2)))
  ;;; automatically make scripts executable on save
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p)
  t)
;(config-default-load)

;;; custom modules
(add-to-list 'load-path "~/.emacs.d/lisp")
;;; prof module
(when (require 'prof nil 'noerror)
  ;(setq prof-dir "~/.emacs.d/prof")
  (prof-files-load-dir "~/.emacs.d/prof")
  (prof-select "google"))
