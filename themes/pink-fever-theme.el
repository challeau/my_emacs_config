;;; pink-fever-theme.el --- a quiet pink color theme for Emacs
;;; --------------------------------------------------
;;; Commentary:
;;; for the girls and the queers
;;; note : fix isearch
;;; --------------------------------------------------
;;; This file is not part of GNU Emacs.
;;; Code:

(deftheme pink-fever
  "Created 2021-09-12.")

(custom-theme-set-faces
 'pink-fever
 ;;; fonts
 '(default ((t (:inherit nil :background "snow" :foreground "pale violet red" :height 98
			 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(fixed-pitch ((t (:family "Monospace"))))
 '(variable-pitch ((((type w32)) (:family "Arial" :foundry "outline")) (t (:family "Sans Serif"))))
 '(escape-glyph ((t (:foreground "plum"))))
 '(homoglyph ((t (:foreground "violet"))))

 ;;; misc basics
 '(cursor ((t (:foreground "misty rose" :background "medium violet red"))))
 '(minibuffer-prompt ((t (:inherit bold :foreground "deep pink" :weight bold))))
 '(vertical-border ((t (:foreground "light pink"))))
 '(header-line ((t (:background "misty rose"))))
 '(tooltip ((t (:background "bisque" :foreground "light salmon"))))
 '(fringe ((nil)))
 '(button ((t (:inherit link))))
 '(link ((t (:underline t :foreground "cornflower blue"))))
 '(link-visited ((t (:underline t :foreground "orchid"))))
 '(warning ((t (:foreground "light slate blue" :inherit bold))))
 '(match ((t (:foreground "lavender blush" :background "light pink" :inherit bold))))

 ;;; highlight and selection
 '(lazy-highlight ((t (:foreground "lavender blush" :background "light pink"))))
 '(highlight ((t (:foreground "hot pink" :background "misty rose" ))))
 '(hl-line ((t (:background "lavender blush"))))
 '(region ((t (:foreground "hot pink" :background "misty rose" :inherit (match)))))
 '(shadow ((t (:foreground "plum"))))
 '(isearch ((t (:foreground "lavender blush" :background "hot pink"))))
 '(isearch-fail ((t (:foreground "lavender blush" :background "plum"))))
 '(isearch-highlight ((t (:foreground "lavender blush" :background "light pink"))))
 '(secondary-selection ((t (:background "seashell"))))
 '(ac-completion-face ((t (:foreground "light salmon" :background "peach puff"))))
 '(next-error ((t (:inherit region))))
 '(query-replace ((t (:inherit isearch))))
 '(highlight-indentation-face ((t (:background "seashell"))))
 '(highlight-symbol-face ((t (:background "papaya whip"))))
 '(highlight-thing ((t (:background "papaya whip"))))
 
 ;;; line numbers
 '(linum ((t (:foreground "pink" :background "lavender blush"))))
 '(line-number ((t (:foreground "pink" :background "snow"))))
 '(line-number-current-line ((t (:foreground "hot pink" :background "snow"))))
 
 ;;; words
 '(trailing-whitespace ((t (:background "peach puff"))))
 '(font-lock-builtin-face ((t (:foreground "orchid"))))
 '(font-lock-comment-delimiter-face ((t (:foreground "pink" :background "seashell"))))
 '(font-lock-comment-face ((t (:slant normal :foreground "light pink" :background "seashell"))))
 '(font-lock-constant-face ((t (:foreground "hot pink"))))
 '(font-lock-doc-face ((t (:foreground "light pink"))))
 '(font-lock-function-name-face ((t (:weight bold :foreground "DeepPink2" :inherit (bold)))))
 '(font-lock-keyword-face ((t (:weight bold :slant normal :foreground "light slate blue"
 				       :inherit (bold)))))
 '(font-lock-negation-char-face ((t (:foreground "deep pink"))))
 '(font-lock-preprocessor-face ((t (:foreground "salmon"))))
 '(font-lock-regexp-grouping-backslash ((t (:foreground "turquoise1"))))
 '(font-lock-regexp-grouping-construct ((t (:foreground "orchid"))))
 '(font-lock-string-face ((t (:weight: bold :foreground "PaleVioletRed1"))))
 '(font-lock-type-face ((t (:foreground "orchid" :inherit (bold)))))
 '(font-lock-variable-name-face ((t (:weight bold :foreground "VioletRed3"))))
 '(font-lock-warning-face ((t (:foreground "peach puff" :background "salmon"))))

 ;;; company
 '(company-echo-common ((t (:foreground "snow" :background "hot pink"))))
 '(company-preview ((t (:foreground "light pink" :background "seashell"))))
 '(company-preview-common ((t (:foreground "light pink" :background "snow"))))
 '(company-preview-search ((t (:inherit match))))
 '(company-scrollbar-bg ((t (:background "seashell"))))
 '(company-scrollbar-fg ((t (:background "light pink"))))
 '(company-template-field ((t (:inherit region))))
 '(company-tooltip ((t (:foreground "light pink" :background "seashell"))))
 '(company-tooltip-annotation ((t (:foreground "light pink"))))
 '(company-tooltip-common ((t (:foreground "orchid" :background "seashell"))))
 '(company-tooltip-common-selection ((t (:foreground "orchid"))))
 '(company-tooltip-mouse ((t (:inherit highlight))))
 '(company-tooltip-search ((t (:inherit match))))
 '(company-tooltip-selection ((t (:foreground "light slate blue" :background "bisque"))))

 ;;; eshell
 '(eshell-ls-archive ((t (:foreground "deep pink" :inherit bold))))
 '(eshell-ls-backup ((t (:foreground "light pink"))))
 '(eshell-ls-clutter ((t (:foreground "light pink"))))
 '(eshell-ls-directory ((t (:foreground "orchid" :inherit bold))))
 '(eshell-ls-executable ((t (:foreground "medium slate blue" :inherit bold))))
 '(eshell-ls-missing ((t (:inherit font-lock-warning-face))))
 '(eshell-ls-product ((t (:inherit font-lock-doc-face))))
 '(eshell-ls-special ((t (:foreground "turquoise" :inherit bold))))
 '(eshell-ls-symlink ((t (:foreground "light green" :inherit bold))))
 '(eshell-ls-unreadable ((t (:foreground "pale violet red"))))
 '(eshell-ls- ((t (:foreground "hot pink" :inherit bold))))
 '(eshell-prompt ((t (:foreground "hot pink" :inherit bold))))

 ;;; flycheck
 '(flycheck-error ((t (:foreground "light sea green" :background "pale turquoise"
				   :inherit bold :underline (:style wave) t))))
 '(flycheck-error-list-checker-name ((t (:foreground "light sea green" :underline t))))
 '(flycheck-fringe-error ((t (:foreground "light sea green"))))
 '(flycheck-info ((t (:foreground "light slate blue" :background "plum"))))
 '(flycheck-fringe-info ((t (:foreground "light slate blue"))))
 '(flycheck-warning ((t (:foreground "salmon" :background "peach puff" :underline nil))))
 '(flycheck-fringe-warning ((t (:foreground "salmon"))))
 '(flycheck-buffer-status-callback ((t (:foreground "light sea green"))))

 ;;; helm
 '(helm-bookmark-directory ((t (:inherit helm-ff-directory))))
 '(helm-bookmark-file ((t (:foreground "pale violet red"))))
 '(helm-bookmark-gnus ((t (:foreground "PaleVioletRed3"))))
 '(helm-bookmark-info ((t (:foreground "PaleVioletRed3"))))
 '(helm-bookmark-man ((t (:foreground "PaleVioletRed3"))))
 '(helm-bookmark-w3m ((t (:foreground "PaleVioletRed3"))))
 '(helm-buffer-directory ((t (:foreground "pale violet red" :background "snow"))))
 '(helm-buffer-file ((t (:foreground "pale violet red" :background "snow"))))
 '(helm-buffer-not-saved ((t (:foreground "PaleVioletRed3" :background "snow"))))
 '(helm-buffer-process ((t (:foreground "light slate blue" :background "snow"))))
 '(helm-buffer-saved-out ((t (:foreground "pale violet red" :background "snow"))))
 '(helm-buffer-size ((t (:foreground "pale violet red" :background "snow"))))
 '(helm-candidate-number ((t (:foreground "light slate blue" :background "snow" :inherit bold))))
 '(helm-ff-directory ((t (:foreground "light slate blue" :background "snow" :inherit bold))))
 '(helm-ff-dotted-directory ((t (:foreground "light slate blue" :background "snow"
					     :inherit bold))))
 '(helm-ff-dotted-symlink-directory ((t (:foreground "turquoise" :background "snow"
						     :inherit bold))))
 '(helm-ff-executable ((t (:foreground "hot pink" :background "snow" :weight normal))))
 '(helm-ff-file ((t (:foreground "hot pink" :background "snow" :weight normal))))
 '(helm-ff-invalid-symlink ((t (:foreground "light green" :background "snow"
					    :inherit bold))))
 '(helm-ff-prefix ((t (:foreground "light slate blue" :background "snow" :weight normal))))
 '(helm-ff-symlink ((t (:foreground "turquoise1" :background "snow" :weight normal))))
 '(helm-grep-cmd-line ((t (:foreground "pale violet red" :background "snow"))))
 '(helm-grep-file ((t (:foreground "pale violet red" :background "snow"))))
 '(helm-grep-finish ((t (:foreground "pale violet red" :background "snow"))))
 '(helm-grep-lineno ((t (:foreground "orchid" :background "snow"))))
 '(helm-grep-match ((t (:foreground nil :background nil :inherit helm-match))))
 '(helm-header ((t (:foreground "pale violet red" :background "snow" :underline nil :box nil))))
 '(helm-header-line-left-margin ((t (:foreground "light slate red" :background nil))))
 '(helm-match ((t (:foreground "hot pink" :background "misty rose"))))
 '(helm-match-item ((t (:foreground "hot pink" :background "misty rose"))))
 '(helm-moccur-buffer ((t (:foreground "orchid" :background "snow"))))
 '(helm-selection ((t (:background "lavender blush"))))
 '(helm-selection-line ((t (:background "misty rose"))))
 '(helm-separator ((t (:foreground "PaleVioletRed3" background "snow"))))
 '(helm-source-header ((t (:foreground "PaleVioletRed3" background "snow" :inherit bold))))
 '(helm-zone-current ((t (:foreground "light slate blue" background "snow"))))
 '(helm-zone-home ((t (:foreground "PaleVioletRed3" background "snow"))))
 '(helm-visible-mark ((t (:foreground "light slate red" background "light pink"))))

 ;;; mode line
 '(mode-line ((t (:box (:line-width 1 :color "light pink") :inverse-video nil
 		       :background "mistyrose" :foreground "hot pink"))))
 '(mode-line-buffer-id ((t (:inherit (bold) :foreground "deep pink"))))
 '(mode-line-emphasis ((t (:weight bold))))
 '(mode-line-highlight ((((class color) (min-colors 88))
 			 (:box (:line-width 2 :color "orchid":style released-button)))
 			(t (:inherit (highlight)))))
 '(mode-line-inactive ((t (:box (:line-width 1 :color "light pink" :style nil)
 				:inverse-video nil :foreground "light pink"
 				:background "lavender blush"))))
 '(telephone-line-accent-active ((t (:foreground "lavender blush" :background "light pink"))))
 '(telephone-line-accent-inactive ((t (:foreground "lavender blush" :background "misty rose"))))
 '(telephone-line-unimportant ((t (:foreground "hot pink"))))
 
 ;;; rainbow-delimiters
 '(rainbow-delimiters-depth-1-face ((t :foreground "violet")))
 '(rainbow-delimiters-depth-2-face ((t :foreground "light slate blue")))
 '(rainbow-delimiters-depth-3-face ((t :foreground "turquoise")))
 '(rainbow-delimiters-depth-4-face ((t :foreground "pale green")))
 '(rainbow-delimiters-depth-5-face ((t :foreground "gold")))
 '(rainbow-delimiters-depth-6-face ((t :foreground "salmon")))
 '(rainbow-delimiters-depth-7-face ((t :foreground "deep pink")))
 '(rainbow-delimiters-depth-8-face ((t :foreground "orchid")))
 '(rainbow-delimiters-mismatched-face ((t :foreground "brown2" :overline t)))
 '(rainbow-delimiters-unmatched-face ((t :foreground "brow2" :overline t)))

 ;;; show-paren
 '(show-paren-match ((t (:foreground "orchid" :inherit bold :underline t))))
 '(show-paren-match-expression ((t (:background "pale green"))))
 '(show-paren-mismatch ((t (:foreground "DeepPink2" :inherit bold))))

 ;;; smartparens
 '(sp-show-pair-match-face ((t (:foreground "orchid" :inherit bold)))))
'(aw-leading-char-face ((t (:foreground "DeepPink2" :background "snow"))))

 (provide-theme 'pink-fever)

;;; pink-fever-theme ends here
