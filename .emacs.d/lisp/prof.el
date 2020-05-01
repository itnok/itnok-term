;;; -*- mode: emacs-lisp; -*-
;;; -*- lexical-binding: nil -*-


(require 'cl)
(require 'cl-lib)
(require 'cc-mode)

;;;
;;; utils
;;;
(defun assoc-val (key alist)
  (cdr (assoc key alist)))

;;;
;;; prof
;;;
(defun prof-get-name (prof)
  (car prof))

;;;
;;; prof-active
;;;
(setq prof-active nil)

(defun prof-unload ()
  (let* ((prof prof-active) (name (car prof)) (attrs (cdr prof)))
    (let ((hooks (assoc-val 'hooks attrs)))
      (cl-loop for hook in hooks collect (remove-hook (car hook) (cdr hook))))
    (let ((finifn (assoc-val 'finifn attrs)))
      (if (null finifn) '() (funcall finifn))))
  (setq prof-active nil)
  t)
(defun prof-load (prof)
  (if (null prof-active) '()
    (prof-unload))
  (let ((name (car prof)) (attrs (cdr prof)))
    (let ((initfn (assoc-val 'initfn attrs)))
      (if (null initfn) '() (funcall initfn)))
    (let ((hooks (assoc-val 'hooks attrs)))
      (cl-loop for hook in hooks collect (add-hook (car hook) (cdr hook)))))
  (setq prof-active prof)
  t)


;;;
;;; profs-list
;;;
(setq profs-list '())

(defun profs-del (name)
  (setq profs-list (cl-remove-if (lambda (p) (equal name (car p))) profs-list))
  profs-list)
(defun profs-set (name rest)
  (setq profs-list (cons (cons name rest) (cl-remove-if (lambda (p) (equal name (car p))) profs-list))))
(defun profs-add (prof)
  (let ((name (car prof)))
    (setq profs-list (cons prof (cl-remove-if (lambda (p) (equal name (car p))) profs-list)))))
(defun profs-get (name)
  (assoc name profs-list))
(defun profs-get-names ()
  (mapcar (lambda (p) (car p)) profs-list))
(defun profs-apply (name)
  (let ((prof (profs-get name)))
    (if (null prof) nil
      (prof-load prof))))



;;;
;;; commands
;;;
(defun prof-select (pname)
  (interactive
   (let* ((completions (profs-get-names))
          (selection (completing-read (format "Select profile: ") completions nil t "")))
     (list selection)))
  (let ((prof (profs-get pname)))
    (if (null prof) '()
      (prof-load prof))))
(defun prof-current ()
  (interactive)
  (message (prof-get-name prof-active)))


;;;
;;; files
;;;
(defun prof-files-load (files)
  (cl-loop for file in files collect (load file)))
(defun prof-files-load-dir (dir)
  (if (not (file-directory-p dir)) '()
    (let* ((entries (directory-files dir t))
           (files (cl-remove-if-not 'file-regular-p entries)))
      (prof-files-load files))))

;;;
;;; module
;;;
(defun prof-module-init ()
  (prof-files-load-dir "~/.emacs.d/prof")
  t)
(defun prof-module-fini ()
  (setq profs-list '())
  (setq prof-active '())
  t)

;(add-hook 'after-init-hook 'prof-module-init)

(profs-add `("none"))

(provide 'prof)
