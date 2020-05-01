(require 'prof)
(defconst prof-alpha-c-style
  `("gnu"
    (indent-tabs-mode . nil)
    (c-basic-offset . 2)
    (c-offsets-alist .
                     ((substatement-open . 0)
                      (access-label . /)
                      (case-label . +)))))
(defun prof-alpha-oninit ()
  (c-add-style "alpha" prof-alpha-c-style))
(defun prof-alpha-onfini ()
  t)
(defun prof-alpha-c-mode-common-hook ()
  (c-set-style "alpha"))
(defun prof-alpha-sh-mode-hook ()
  (setq sh-basic-offset 2))
(defun prof-alpha-python-mode-hook ()
  (setq python-indent 2))
(defconst prof-alpha
  `("alpha"
    (initfn . prof-alpha-oninit)
    (finifn . prof-alpha-onfini)
    (hooks . ((c-mode-common-hook . prof-alpha-c-mode-common-hook)
              (sh-mode-hook       . prof-alpha-sh-mode-hook)
              (python-mode-hook   . prof-alpha-python-mode-hook)))))
(profs-add prof-alpha)
(provide 'prof-alpha)
