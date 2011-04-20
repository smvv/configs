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

; Remove trailing whitespace on buffer save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

; Git support
(load "git.el")
;(load "/usr/share/doc/git-core/contrib/emacs/git-blame.el")
(load "vc-git.el")
(add-to-list 'vc-handled-backends 'GIT)

; Snippets
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")

; Automatic refilling of text or code, based on line width
(setq-default fill-column 66)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;Load flymake for syntax checking (and pyflakes for python syntax).
;(when (load "flymake" t)
; (defun flymake-pyflakes-init ()
;  (let* ((temp-file (flymake-init-create-temp-buffer-copy
;                              'flymake-create-temp-inplace))
;    (local-file (file-relative-name temp-file-name-directory buffer-file-name))))
;; (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                       'flymake-create-temp-inplace))
;;         (local-file (file-relative-name temp-file (file-name-directory buffer-file-name))))
;   (list "pyflakes" (list local-file))))
; (add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-pyflakes-init))
;(add-hook 'find-file-hook 'flymake-find-file-hook)

;; Auto Syntax Error Hightlight
(when (load "flymake" t)
  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		              'flymake-create-temp-inplace))
	      (local-file (file-relative-name
			   temp-file
			   (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
	              '("\\.py\\'" flymake-pyflakes-init)))
(add-hook 'find-file-hook 'flymake-find-file-hook)
(provide 'init_python)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(vc-follow-symlinks t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )
