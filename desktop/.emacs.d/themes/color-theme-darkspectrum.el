;; Darkspectrum Colour Theme for Emacs.
;;
;; Defines a colour scheme resembling that of the original TextMate Blackboard colour theme.
;; To use add the following to your .emacs file (requires the color-theme package):
;;
;; (require 'color-theme)
;; (color-theme-initialize)
;; (load-file "~/.emacs.d/themes/color-theme-blackboard.el")
;;
;; And then (color-theme-blackboard) to activate it.
;;
;; MIT License Copyright (c) 2008 JD Huntington <jdhuntington at gmail dot com>
;; Credits due to the excellent TextMate Blackboard theme
;;
;; All patches welcome

(defun color-theme-darkspectrum()
  "Color theme by JD Huntington, based off the TextMate Blackboard theme, created 2008-11-27"
  (interactive)
  (color-theme-install
   '(color-theme-darkspectrum
     (;(background-color . "#0C1021")
      (background-mode . dark)
      (border-color . "black")
      (cursor-color . "#A7A7A7")
      (foreground-color . "#F8F8F8")
      (mouse-color . "sienna1"))
     (default ((t (:background "#-1C1021" :foreground "#F8F8F8"))))
     (blue ((t (:foreground "#3C3CFF"))))
     (bold ((t (:bold t))))
     (bold-italic ((t (:bold t))))
     (border-glyph ((t (nil))))
     (buffers-tab ((t (:background "#0C1021" :foreground "#F8F8F8"))))
     (font-lock-builtin-face ((t (:foreground "#F8F8F8"))))
     (font-lock-comment-face ((t (:italic t :foreground "#8A8A8A"))))
     (font-lock-constant-face ((t (:foreground "#EF5939"))))
     (font-lock-doc-string-face ((t (:foreground "DarkOrange"))))
     (font-lock-function-name-face ((t (:foreground "#AD7FA8"))))
     (font-lock-keyword-face ((t (:bold t :foreground "#FFFFFF"))))
     (font-lock-preprocessor-face ((t (:foreground "Aquamarine"))))
     (font-lock-reference-face ((t (:foreground "SlateBlue"))))

     (font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))
     (font-lock-regexp-grouping-construct ((t (:foreground "red"))))

     (font-lock-string-face ((t (:foreground "#FCE94F"))))
     (font-lock-type-face ((t (:foreground "#8AE234"))))
     (font-lock-variable-name-face ((t (:foreground "#729FCF"))))
     (font-lock-warning-face ((t (:bold t :foreground "Pink"))))

     (flymake-errline-face ((t (:foreground "red" :underline t
					    :background nil))))
     (flymake-warnline-face ((t (:foreground "orange" :underline t
					     :background nil))))
     (flymake-errline ((((class color) (background light))
			(:inherit flymake-errline-face))))
     (flymake-warnline ((((class color) (background light))
			 (:inherit flymake-warnline-face))))

     (secondary-selection ((((class color) (min-colors 88) (background light))
			    (:background "yellow1" :foreground "#666"))))

     (diff-added ((t (:inherit diff-changed :foreground "green"))))
     (diff-file-header ((((class color) (min-colors 88) (background light))
			 (:weight bold))))
     (diff-header ((((class color) (min-colors 88) (background light)) nil)))
     (diff-refine-change ((((class color) (min-colors 88) (background light))
			   nil)))
     (diff-removed ((t (:inherit diff-changed :foreground "red"))))
     (font-latex-doctex-documentation-face ((t nil)))
     (font-latex-sectioning-5-face ((((type tty pc) (class color)
				      (background light))
				     (:foreground "blue" :weight bold))))
     (fringe ((((class color) (background light)) nil)))
     (isearch-fail ((((class color) (min-colors 88) (background light))
		     (:background "RosyBrown4"))))
     (lazy-highlight ((((class color) (min-colors 88) (background light))
		       (:background "Turquoise4"))))
     (match ((((class color) (min-colors 88) (background light))
	      (:background "yellow3"))))
     (preview-face ((t nil)))
     (yas/field-highlight-face ((((class color) (background light))
				 (:background "DarkSeaGreen4"))))

     (gui-element ((t (:background "#D4D0C8" :foreground "black"))))
     (minibuffer-prompt ((t (:foreground "#6690ff"))))
     (region ((t (:background "#253B76"))))
     (mode-line ((t (:foreground "#cccccc"))))
     (mode-line-inactive ((t (:background nil :foreground "#888888"))))
     (highlight ((t (:background "#222222"))))
     (highline-face ((t (:background "SeaGreen"))))
     (italic ((t (nil))))
     (left-margin ((t (nil))))
     (text-cursor ((t (:background "yellow" :foreground "black"))))
     (toolbar ((t (nil))))
     (underline ((nil (:underline nil))))
     (zmacs-region ((t (:background "snow" :foreground "blue")))))))
