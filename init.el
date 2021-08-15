;;; init.el --- my first init file
;;; --------------------------------------------------
;;; Commentary:
;;; it ain't much but it's honest work
;;; --------------------------------------------------
;;; Code:

;;; --------------------------------------------------
;;; START-UP
;;; --------------------------------------------------
;;; packages

(require 'package)
;;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("elpy" . "https://melpa.org/#/elpy") t)
(add-to-list 'package-archives '("yasnippet" . "https://melpa.org/#/yasnippet") t)

(when (not package-archive-contents)
  (package-refresh-contents)
  )

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;;; --------------------------------------------------
;;; STARTUP
;;; --------------------------------------------------
;;; no message, menu/tool bar, font menu, and ring. replace (yes, no) questions with (y,n).
(setq inhibit-startup-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(setq ring-bell-function 'ignore)
(fset 'yes-or-no-p 'y-or-n-p)

;;; windows config
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
(split-window-right)
(windmove-right)
(split-window)
(windmove-down)
(eshell)
(windmove-left)

;;; --------------------------------------------------
;;; PLUGINS
;;; --------------------------------------------------
(use-package ace-jump-mode
  :bind ("C-x j" . 'ace-jump-mode)
  :bind ("C-x l" . 'ace-jump-line-mode))

(use-package ace-window
  :bind ("C-x o" . 'ace-window)
  )

(use-package company
  :config (global-company-mode)
  )

(use-package elpy
  :init (elpy-enable)
  )

(use-package flycheck
  :config (global-flycheck-mode)
  )

(use-package recentf
  :bind ("C-x C-r" . 'recentf-open-files)
  :config (recentf-mode 1)
  (setq-default recent-save-file "~/.emacs.d/recentf")
  (setq recentf-max-menu-items 40)
  )

(use-package helm
  :config (recentf-mode 1)
  (setq-default recent-save-file "~/.emacs.d/recentf")
  (setq-default helm-ff-file-name-history-use-recentf t)
  :bind (("C-x b" . 'helm-buffers-list)
	 ("C-x C-f" . 'helm-find-files))
  )

(use-package org
  :bind (("C-c l" . 'org-store-link)
	 ("C-c a" . 'org-agenda))
  :config (setq org-log-done t)
  )


(use-package saveplace
  :config (save-place-mode 1)
  )

(use-package smartparens
  :config (smartparens-global-mode)
  (show-smartparens-global-mode)
  )

(use-package smex
  :bind ("M-x" . smex)
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize)
  )

(use-package yasnippet
  :init (yas-global-mode 1)
  :config (yas-reload-all)
  )

;;; --------------------------------------------------
;;; KEY BINDINGS
;;; --------------------------------------------------
;;; duplicate line
(global-set-key (kbd "C-c d")
		(lambda () (interactive)
		  (move-beginning-of-line 1) (kill-line) (yank) (newline) (yank))
		)

;;; copy whole line
(global-set-key (kbd "C-c w")
		(lambda () (interactive)
		  (move-beginning-of-line 1) (set-mark-command nil) (forward-line)
		  (kill-ring-save (region-beginning) (region-end)))
		)

;;; toggle comment on region
(global-set-key (kbd "M-;") 'comment-or-uncomment-region)

;;; toggle comment on line
(global-set-key (kbd "C-c C-c")
		(lambda () (interactive)
		  (move-beginning-of-line 1) (set-mark-command nil) (forward-line)
		  (comment-or-uncomment-region (region-beginning) (region-end)))
		)

;;; --------------------------------------------------
;;; LOOKS
;;; --------------------------------------------------
;;; theme
(unless (package-installed-p 'gruvbox-theme)
  (package-refresh-contents)
  (package-install 'gruvbox-theme)
  )


;;; theme choice, courtesy of Lureif
(defun select-theme ()
  "Asks the user if they want to load a light or a dark theme."
  (if (y-or-n-p "Load light theme ? ")
      (load-theme 'gruvbox-light-medium t)
    (load-theme 'gruvbox-dark-medium t))
  )
(select-theme)

;;; mode line
(use-package telephone-line
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left)
  (setq telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left)
  (setq telephone-line-primary-right-separator 'telephone-line-cubed-left)
  (setq telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-left)
  (setq telephone-line-height 24
	telephone-line-evil-use-short-tag t)
  (telephone-line-mode 1)
  )

;;; --------------------------------------------------
;;; EDITING
;;; --------------------------------------------------
;;; line number and highlght
(global-linum-mode)
(global-hl-line-mode 1)

;;; killing/yanking interact with the clipboard
(setq select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t
      )

;;; parentheses highlight and colour
(show-paren-mode 1)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;; --------------------------------------------------
;;; MODE SPECIFIC
;;; --------------------------------------------------
;;; shell scripts indentation
(setq-default sh-basic-offset 2
	      sh-indentation 2)

;;; c mode style/indentation
(defvar c-default-style "linux")
(setq-default indent-tabs-mode t)
(defvaralias 'c-basic-offset 'tab-width)
(add-hook 'c-mode-hook
  (function (lambda ()
              (whitespace-mode t))))

;;; clojure/elisp setup
(add-to-list 'load-path "~/.emacs.d/customizations")
(load "setup-clojure.el")
(load "setup-js.el")
(load "elisp-editing.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("3c68f48ea735abe65899f489271d11cbebbe87da7483acf9935ea4502efd0117" "fe1c13d75398b1c8fd7fdd1241a55c286b86c3e4ce513c4292d01383de152cb7" "939ea070fb0141cd035608b2baabc4bd50d8ecc86af8528df9d41f4d83664c6a" "a06658a45f043cd95549d6845454ad1c1d6e24a99271676ae56157619952394a" "4cf9ed30ea575fb0ca3cff6ef34b1b87192965245776afa9e9e20c17d115f3fb" "e1d09f1b2afc2fed6feb1d672be5ec6ae61f84e058cb757689edb669be926896" "aded61687237d1dff6325edb492bde536f40b048eab7246c61d5c6643c696b7f" default)))
 '(package-selected-packages
   (quote
    (spaceline twilight-anti-bright-theme dracula-theme dumb-jump helm ace-window ace-jump-mode elpy sublimity evil telephone-line yasnippet flycheck company powerline use-package tagedit smex rainbow-delimiters projectile paredit magit ido-completing-read+ exec-path-from-shell clojure-mode-extra-font-locking cider))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;--------------------------------------------------
(provide 'init)
;;; init.el ends here
