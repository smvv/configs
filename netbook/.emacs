;(load-file "~/.emacs.d/plugins/byte-code-cache.el")

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
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

; Show line-number in the mode line
(line-number-mode 1)

; Show column-number in the mode line
(column-number-mode 1)

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
  (defun flymake-create-temp-intemp (file-name prefix)
   "Return file name in temporary directory for checking FILE-NAME.
This is a replacement for `flymake-create-temp-inplace'. The
difference is that it gives a file name in
`temporary-file-directory' instead of the same directory as
FILE-NAME.

For the use of PREFIX see that function.

Note that not making the temporary file in another directory
\(like here) will not if the file you are checking depends on
relative paths to other files \(for the type of checks flymake
makes)."
   (unless (stringp file-name) (error "Invalid file-name"))
   (or prefix (setq prefix "flymake"))
   (let* ((name (concat
                 (file-name-nondirectory
                 (file-name-sans-extension file-name))
                 "_" prefix))
          (ext  (concat "." (file-name-extension file-name)))
          (temp-name (make-temp-file name nil ext))
         )
     (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
      temp-name)
   )

  (defun flymake-pyflakes-init ()
    (let* ((temp-file (flymake-init-create-temp-buffer-copy
		              'flymake-create-temp-intemp))
	      (local-file (file-relative-name
			   temp-file
			   (file-name-directory buffer-file-name))))
      (list "pyflakes" (list local-file))))
  (add-to-list 'flymake-allowed-file-name-masks
	              '("\\.py\\'" flymake-pyflakes-init)))
(add-hook 'find-file-hook 'flymake-find-file-hook)
(provide 'init_python)

(load-file "~/.emacs.d/plugins/flymake-err-msg.el")

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
 '(diff-added ((t (:inherit diff-changed :foreground "green"))))
 '(diff-file-header ((((class color) (min-colors 88) (background light)) (:weight bold))))
 '(diff-header ((((class color) (min-colors 88) (background light)) nil)))
 '(diff-refine-change ((((class color) (min-colors 88) (background light)) nil)))
 '(diff-removed ((t (:inherit diff-changed :foreground "red"))))
 '(flymake-errline ((t (:foreground "red" :underline t))))
 '(flymake-warnline ((((class color) (background light)) (:foreground "orange" :underline t))))
 '(font-latex-doctex-documentation-face ((t nil)))
 '(font-latex-sectioning-5-face ((((type tty pc) (class color) (background light)) (:foreground "blue" :weight bold))))
 '(fringe ((((class color) (background light)) nil)))
 '(isearch-fail ((((class color) (min-colors 88) (background light)) (:background "RosyBrown4"))))
 '(lazy-highlight ((((class color) (min-colors 88) (background light)) (:background "Turquoise4"))))
 '(match ((((class color) (min-colors 88) (background light)) (:background "yellow3"))))
 '(preview-face ((t nil)))
 '(yas/field-highlight-face ((((class color) (background light)) (:background "DarkSeaGreen4"))))
 '(zmacs-region ((t (:foreground "blue")))))
