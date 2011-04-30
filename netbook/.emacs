;;(load-file "~/.emacs.d/plugins/byte-code-cache.el")

;; ---------------------------------------------------------------------------
;; Startup customizations
;; ---------------------------------------------------------------------------

;; This should be the first custom settings, because if emacs is not able to
;; load this config correctly, the default white background hides the almost
;; fully white text.
(set-background-color "black")

;; Don't add new lines to the end of a file when using down-arrow key
;;(setq next-line-add-newlines nil)

;; Dont show the GNU splash screen
(setq inhibit-startup-message t)

;; Initialise dark color theme
(require 'color-theme)
(color-theme-initialize)
(load-file "~/.emacs.d/themes/color-theme-darkspectrum.el")
(color-theme-darkspectrum)

;; arg >= 1 enable the menu bar.p
(menu-bar-mode 0)

;; Make all "yes or no" prompts show "y or n" instead
(set 'yes-or-no-p 'y-or-n-p)

;; Show line-number in the mode line
(line-number-mode 1)

;; Show column-number in the mode line
(column-number-mode 1)

;; ---------------------------------------------------------------------------
;; Shortcuts
;; ---------------------------------------------------------------------------

(global-set-key [f1] 'open-dot-emacs)
;;(global-set-key [f2] 'comment-region) ; default: C-M-\
(global-set-key [(shift f2)] 'universal-argument) ;uncomment is Shift-F2 F2
(global-set-key [f3] 'indent-region)
(global-set-key [f4] 'shell)
(global-set-key [f5] 'compile)
(global-set-key [f11] 'undo)

;; ---------------------------------------------------------------------------
;; Whitespace handling
;; ---------------------------------------------------------------------------

;; Don't wrap long lines.
(set-default 'truncate-lines t)

;; Remove trailing whitespace on buffer save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Automatic refilling of text or code, based on line width
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; ---------------------------------------------------------------------------
;; Git support
;; ---------------------------------------------------------------------------

(load "git.el")
;;(load "/usr/share/doc/git-core/contrib/emacs/git-blame.el")
(load "vc-git.el")
(add-to-list 'vc-handled-backends 'GIT)

;; Follow symbolic links of files on load (e.g. emacs config file).
(setq vc-follow-symlinks t)

;; Snippets support
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/.emacs.d/snippets")

;; ---------------------------------------------------------------------------
;; Highlighting
;; ---------------------------------------------------------------------------

;; highlight region between point and mark
(transient-mark-mode t)

;; highlight during query
(setq query-replace-highlight t)

;; highlight incremental search
(setq search-highlight t)

;; Show matching parenthesis. How can you live without it.
(show-paren-mode t)

;; ---------------------------------------------------------------------------
;; Gnus settings
;; ---------------------------------------------------------------------------

(defun gnus-init ()
  ;; Start Gnus when Emacs starts
  (add-hook 'emacs-startup-hook 'gnus t)
  ;; Exit Emacs after quitting Gnus
  (add-hook 'gnus-after-exiting-gnus-hook
	    'save-buffers-kill-emacs))

;; ---------------------------------------------------------------------------
;; Syntax checking
;; ---------------------------------------------------------------------------

;;Load flymake for syntax checking (and pyflakes for python syntax).
;;(when (load "flymake" t)
;; (defun flymake-pyflakes-init ()
;;  (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;                              'flymake-create-temp-inplace))
;;    (local-file (file-relative-name temp-file-name-directory buffer-file-name))))
;;;; (let* ((temp-file (flymake-init-create-temp-buffer-copy
;;;;                       'flymake-create-temp-inplace))
;;;;         (local-file (file-relative-name temp-file (file-name-directory buffer-file-name))))
;;   (list "pyflakes" (list local-file))))
;; (add-to-list 'flymake-allowed-file-name-masks '("\\.py\\'" flymake-pyflakes-init))
;;(add-hook 'find-file-hook 'flymake-find-file-hook)

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
	   (temp-name (make-temp-file name nil ext)))
      (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
      temp-name))

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

;; Dispaly flymake warnings and errors at the minibuffer.
(load-file "~/.emacs.d/plugins/flymake-err-msg.el")
