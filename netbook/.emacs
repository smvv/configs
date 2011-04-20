; Initialise dark color theme
(require 'color-theme)
 (color-theme-initialize)
 (load-file "~/.emacs.d/themes/color-theme-darkspectrum.el")
 (color-theme-darkspectrum)

; arg >= 1 enable the menu bar.p
(menu-bar-mode 0)

; Make all "yes or no" prompts show "y or n" instead
(fset 'yes-or-no-p 'y-or-n-p)

; Don't wrap long lines.
(set-default 'truncate-lines t)

;Load flymake for syntax checking (and pyflakes for python syntax).
(when (load "flymake" t) 
 (defun flymake-pyflakes-init () 
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
                              'flymake-create-temp-inplace))
    (local-file (file-relative-name temp-file-name-directory buffer-file-name))))
; (let* ((temp-file (flymake-init-create-temp-buffer-copy
;                       'flymake-create-temp-inplace))
;         (local-file (file-relative-name temp-file (file-name-directory buffer-file-name))))
   (list "pyflakes" (list local-file))))
 (add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-pyflakes-init))
(add-hook 'find-file-hook 'flymake-find-file-hook)
