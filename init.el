;;; init.el --- my first init file
;;; --------------------------------------------------
;;; Commentary:
;;; it ain't much but it's honest work
;;; --------------------------------------------------
;;; Code:

;;; packages

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
  (add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'package-archives
                '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
  (add-to-list 'package-pinned-packages '(magit . "melpa-stable") t)
  (add-to-list 'package-pinned-packages '(evil  . "melpa-stable") t)

  )

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents)
  )

(defvar my-packages
  '(
    cider
    clojure-mode
    clojure-mode-extra-font-locking
    company
    evil
    flycheck
    magit
    paredit
    rainbow-delimiters
    smex
    telephone-line
    yasnippet)
  )

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p))
  )

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
  )

(require 'use-package)
(setq use-package-always-ensure t)

(use-package company
  :ensure t
  :config (global-company-mode)
  )

(require 'evil)
  (evil-mode 1)

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode)
  )

(require 'yasnippet)
(use-package yasnippet
   :ensure t
   :config
   (yas-global-mode t)
   (yas-reload-all)
   )

;;; STARTUP
;;; no message
(setq inhibit-startup-message t)

;;; windows config
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)
(split-window-right)
(windmove-right)
(split-window)
(windmove-down)
(eshell)
(windmove-left)

;;; no menu/tool bars
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)

;;; no ring
(setq ring-bell-function 'ignore)

;;; no font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;;; LOOKS
;;; theme
(unless (package-installed-p 'gruvbox-theme)
  (package-refresh-contents)
  (package-install 'gruvbox-theme)
  )

;;; theme choice, function is courtesy of Lureif
(defun select-theme ()
  "Asks the user if they want to load a light or a dark theme."
  (if (y-or-n-p "Load light theme ? ")
      (load-theme 'gruvbox-light-medium t)
    (load-theme 'gruvbox-dark-medium t)))
(select-theme)

;;; mode line
(require 'telephone-line)
(setq telephone-line-primary-left-separator 'telephone-line-cubed-left
      telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left
      telephone-line-primary-right-separator 'telephone-line-cubed-left
      telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-left)
(setq telephone-line-height 24
      telephone-line-evil-use-short-tag t)
(telephone-line-evil-config)
(telephone-line-mode 1)


;;; NAVIGATION
;;; remembers recent commands
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)

;; point to the last place where it was when you the file was last visited
(require 'saveplace)
(setq-default save-place t)

;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;;; insensitivity to case in find file and eshell
(setq read-file-name-completion-ignore-case t)
(setq eshell-glob-case-insensitive t)
(setq pcomplete-ignore-case t)

;;; (yes or no) questions replace with (y or n)
(fset 'yes-or-no-p 'y-or-n-p)

;;; easy browsing with M-x
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)


;;; EDITING
;;; line number and highlght
(global-linum-mode)
(global-hl-line-mode 1)

;;; killing/yanking interact with the clipboard
(setq
      select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t)

;;; parentheses highlight and colour
(show-paren-mode 1)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;; backup files in a directory
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

;;; disables the creation of # files during edition (or is it ~ files?)
(setq auto-save-default nil)

;;; MODE SPECIFIC
;;; shell scripts indentation
(setq-default sh-basic-offset 2
	      sh-indentation 2)

;;; c mode style/indentation
(setq c-default-style "linux")
(setq-default c-basic-offset 4)

;;; bash-like completion in eshell
(setq eshell-cmpl-cycle-completions nil)

;;; clojure/elisp setup
(add-to-list 'load-path "~/.emacs.d/customizations")
(load "setup-clojure.el")
(load "setup-js.el")
(load "elisp-editing.el")


;;; Ease OF USE
;;; key bindings
(global-set-key (kbd "C-x c") 'kill-ring-save)
(global-set-key (kbd "C-x x") 'kill-region)
(global-set-key (kbd "C-x v") 'yank)

(global-set-key (kbd "M-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c C-c") 'comment-line)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("e1d09f1b2afc2fed6feb1d672be5ec6ae61f84e058cb757689edb669be926896" "aded61687237d1dff6325edb492bde536f40b048eab7246c61d5c6643c696b7f" default)))
 '(package-selected-packages
   (quote
    (evil telephone-line yasnippet flycheck company powerline use-package tagedit smex rainbow-delimiters projectile paredit magit ido-completing-read+ exec-path-from-shell clojure-mode-extra-font-locking cider))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;--------------------------------------------------
(provide 'init)
;;; init.el ends here
