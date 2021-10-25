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
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

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
(eshell)
(windmove-left)

;;; --------------------------------------------------
;;; PLUGINS
;;; --------------------------------------------------
(use-package ace-jump-mode
  :bind ("C-x j" . 'ace-jump-mode)
  :bind ("C-x l" . 'ace-jump-line-mode))

(use-package ace-window
  :bind ("C-x o" . 'ace-window))

(use-package company
  :config (global-company-mode))

(use-package elpy
  :init (elpy-enable))

(use-package flycheck
  :config (global-flycheck-mode))

(use-package recentf
  :bind ("C-x C-r" . 'recentf-open-files)
  :config (recentf-mode 1)
  (setq-default recent-save-file "~/.emacs.d/recentf")
  (setq recentf-max-menu-items 40))

(use-package helm
  :config (recentf-mode 1)
  (setq-default recent-save-file "~/.emacs.d/recentf")
  (setq-default helm-ff-file-name-history-use-recentf t)
  :bind (("C-x b" . 'helm-buffers-list)
	 ("C-x C-f" . 'helm-find-files)))

(use-package org
  :bind (("C-c l" . 'org-store-link)
	 ("C-c a" . 'org-agenda))
  :config (setq org-log-done t))

(use-package saveplace
  :config (save-place-mode 1))

(use-package smartparens
  :config (smartparens-global-mode)
  (show-smartparens-global-mode)
  :hook
  (c-mode-common . turn-off-smartparens-strict-mode))

(use-package smex
  :bind ("M-x" . smex)
  :config
  (setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize))

(use-package yasnippet
  :init (yas-global-mode 1)
  :config (yas-reload-all)
  (setq mode-require-final-newline nil))

;;; --------------------------------------------------
;;; KEY BINDINGS
;;; --------------------------------------------------
;;; duplicate line
(global-set-key (kbd "C-c d")
		(lambda () (interactive)
		  (move-beginning-of-line 1) (kill-line) (yank) (newline) (yank))
		)

;;; copy whole line
(global-set-key (kbd "M-W")
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
(let ((basedir "~/.emacs.d/themes/"))
  (dolist (f (directory-files basedir))
    (if (and (not (or (equal f ".") (equal f "..")))
             (file-directory-p (concat basedir f)))
        (add-to-list 'custom-theme-load-path (concat basedir f)))))

(unless (package-installed-p 'gruvbox-theme)
  (package-refresh-contents)
  (package-install 'gruvbox-theme))

(unless (package-installed-p 'dracula-theme)
  (package-refresh-contents)
  (package-install 'dracula-theme))

;;; theme choice, courtesy of Lureif
;;;(defun select-theme ()
;;;  "Asks the user if they want to load a light or dark theme."
;;;  (if (y-or-n-p "Load light theme ? ")
;;;      (load-theme 'gruvbox-light-medium t)
;;;    (load-theme 'dracula t))
;;;  )
;;; (select-theme)

(require 'ido)
(defun pick-theme ()
  "Prompt user to pick a light, dark, or pink theme."
  (interactive)
  (let ((fav-themes '("pink" "dark" "light")))
    (defvar theme-choice)
    (setq theme-choice (message "%s"(ido-completing-read "Please select a theme:" fav-themes))))
  (if (string= theme-choice "pink")
      (load-theme 'pink-fever t))
  (if (string= theme-choice "dark")
      (load-theme 'dracula t))
  (if (string= theme-choice "light")
      (load-theme 'spacemacs-light t)))

(pick-theme)

;;; mode line
(use-package telephone-line
  :config
  (setq telephone-line-primary-left-separator 'telephone-line-cubed-left)
  (setq telephone-line-secondary-left-separator 'telephone-line-cubed-hollow-left)
  (setq telephone-line-primary-right-separator 'telephone-line-cubed-left)
  (setq telephone-line-secondary-right-separator 'telephone-line-cubed-hollow-left)
  (setq telephone-line-height 24
	telephone-line-evil-use-short-tag t)
  (telephone-line-mode 1))

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

(setq mode-require-final-newline nil)

;; (add-hook 'c-mode-hook
;;   (function (lambda ()
;;               (whitespace-mode t))))

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
 '(CUA-mode-read-only-cursor-color "dark grey")
 '(ansi-color-names-vector
   ["#d2ceda" "#f2241f" "#67b11d" "#b1951d" "#3a81c3" "#a31db1" "#21b8c7" "#655370"])
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("03d3919e7bf6ea0b56f1d60a369f281dadbca56d26e024994e09f20eded1ca28" "effe3d3b40454409a770838a6db3444bd352b07008e2fffb4c439f18cdaa414b" "ab63e1840db010e5063f500d11f5c85e4fe2a2511820c27eb5b8cc5be7364848" "d590474309fb8cf36c44bc8ee33712859560d8d358b19b1217f8528216df490e" "195fd81884699d5603040997ab956f5e2a897e87b157b5e40463e38178249d96" "fd0ad4c9870ca21884cc4ac3e6b35ede7ba04ee14c23dea3835d5d916423f34c" "42f4a8370c10ad32c9eb86b801d06c3fb58b8d6bda4e527c03ec3d06d760db9b" "a2d6bba16952ca6b5323c5ec3d51ec967eb6f053a26bdc4b197216e35eb0cb5a" "16df0e824bd3aca8ffc98143df5445b19e3dcc9c6f4cd9391691b60896ff420d" "6634bc1ea0ac32c45280cf05358ee00ea2fd37bbad2771d3b5677713dd3fb6e8" "aeddcde4695df0d2bbbe5122333fa77965fed1251077006e845432521bc1bed2" "b7025936f05a782c6181bf9672f1b4a0c03312816e70789b23c69dec57d9fa8b" "3d7787f3479a1d95dcf38698d13b0258e5438c6bc94ac8a446d15fc67061f5f6" "212df371f4f0908eef441e225a209807e3f6f20d1e2d52a693439213f909a4f8" "1c00f7ea2aeac86ebf00f42bab5a75dd315fcada5e8ef2c034315e2dc337a876" "1665a9e2548ea225eddeaa0a91b868cd2acdb3f2e01372e3f5fcb7157fdd71ed" "0e59b6f5aea1fe8c14e3368e8e035fcfaeb5b2e15b39bf295aa49020d3caf472" "50648bcfaa3702766a679dc75245d4c9c05a96cab50972e41e583ba0e0eb29f7" "2d678a931efb0959f72f644639d92c12950b3346a1a9185f2360619a9219bfcc" "3ac9d6a254da4f53959658bab0a05ff795965f34458d4f8afe2057626bcaf1e8" "fa6e7c0fb22600e0624269de86607447b3c88e4fd593a50f390210df6052a7aa" "9e54a6ac0051987b4296e9276eecc5dfb67fdcd620191ee553f40a9b6d943e78" "5ee12d8250b0952deefc88814cf0672327d7ee70b16344372db9460e9a0e3ffc" "1157a4055504672be1df1232bed784ba575c60ab44d8e6c7b3800ae76b42f8bd" "3647aa8082ce637a16f2b63578388d83c6d0db52dafdac9d0045e0fb682ba5fb" "7e07d4463f6cc1d7bbf364b2495c1bab32c883c0aa6bbe9b209bdc1724fcdf4d" "76b4632612953d1a8976d983c4fdf5c3af92d216e2f87ce2b0726a1f37606158" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "d548ac4bb4c8c0ba8f22476f5afcea11b7f1754065eefb118e1324f8a74883fb" "5642b25b6df4d6b63787cbc3d3ef07ca4cb7b0a7a00740ce8e9867c00e57632f" "3c68f48ea735abe65899f489271d11cbebbe87da7483acf9935ea4502efd0117" "fe1c13d75398b1c8fd7fdd1241a55c286b86c3e4ce513c4292d01383de152cb7" "939ea070fb0141cd035608b2baabc4bd50d8ecc86af8528df9d41f4d83664c6a" "a06658a45f043cd95549d6845454ad1c1d6e24a99271676ae56157619952394a" "4cf9ed30ea575fb0ca3cff6ef34b1b87192965245776afa9e9e20c17d115f3fb" "e1d09f1b2afc2fed6feb1d672be5ec6ae61f84e058cb757689edb669be926896" "aded61687237d1dff6325edb492bde536f40b048eab7246c61d5c6643c696b7f" default)))
 '(custom-theme-directory "~/.emacs.d/themes")
 '(help-highlight-face (quote info-xref))
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#3a81c3")
     ("OKAY" . "#3a81c3")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#42ae2c")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f"))))
 '(list-matching-lines-buffer-name-face (quote bold))
 '(package-selected-packages
   (quote
    (## mood-line twilight-bright-theme spacemacs-theme flucui-themes spaceline twilight-anti-bright-theme dracula-theme dumb-jump helm ace-window ace-jump-mode elpy sublimity evil telephone-line yasnippet flycheck company powerline use-package tagedit smex rainbow-delimiters projectile paredit magit ido-completing-read+ exec-path-from-shell clojure-mode-extra-font-locking cider)))
 '(pdf-view-midnight-colors (quote ("#655370" . "#fbf8ef")))
 '(rcirc-colors pink-bliss-foreground-colors)
 '(smartparens-global-strict-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;--------------------------------------------------
(provide 'init)
;;; init.el ends here
