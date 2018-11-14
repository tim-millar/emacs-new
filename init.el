;; load package and set archives
(require 'package)

(setq package-enable-at-startup nil)

(setq package-archives '(
			 ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 )
      package-archive-priorities '(("melpa" . 1))
      )

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(require 'json)

(use-package paradox
  :ensure t
  :diminish (paradox-menu-mode . "Paradox")
  :config
  (paradox-enable))

(use-package try
  :ensure t
  :commands try)

;; ==============================
;; Appearance
;; ==============================

(set-frame-font "Hack 13")

;; fix annoying defaults

(setq inhibit-splash-screen t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)

(show-paren-mode t)
(blink-cursor-mode -1)
(global-hl-line-mode t)

(setq column-number-mode t
      sentence-end-double-space nil
      default-fill-column 80
      default-indent-tabs-mode nil)

(defalias 'list-buffers 'ibuffer)

(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'kill-emacs-query-functions
	  (lambda () (y-or-n-p "Do you really want to exit Emacs? "))
	  'append)

;; file backups
(setq backup-directory-alist
      '(("." . "~/.emacs.d.new/backups"))
      auto-save-file-name-transforms
      `((".*" "~/.emacs.d.new/auto-save-list/" t))
      backup-by-copying t)

;; themes, modeline and icons

(defun tm/switch-theme (theme)
  ;; This interactive call is taken from `load-theme'
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))))
  (mapcar #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(use-package all-the-icons
  :ensure t
  :config
  (setq all-the-icons-color-icons t))

(use-package doom-themes
  :ensure t
  :init
  (load-theme 'doom-dracula t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :defer t
  :init
  (doom-modeline-init)
  :config
  (setq doom-modeline-buffer-file-name-style 'relative-to-project))

(use-package solaire-mode
  :ensure t
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  :config
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
  (solaire-mode-swap-bg))

;; ==============================
;; UI
;; ==============================

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (which-key-mode)
  :bind
  (("<f5>" . which-key-show-top-level))
  :config
  (which-key-setup-side-window-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
	which-key-side-window-max-width 0.33
	which-key-idle-delay 0.75))

;; ==============================
;; Evil
;; ==============================

(use-package evil
  :ensure t
  :config
  (setq evil-disable-insert-state-bindings t)
  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'inf-ruby-mode 'emacs)
  (evil-set-initial-state 'commint-mode 'normal)
  (evil-mode 1))


;; ==============================
;; Version Controll
;; ==============================

(use-package magit
  :ensure t)

(use-package evil-magit
  :ensure t
  :after (evil magit)
  :config
  (setq evil-magit-state 'normal))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (evil-magit magit solaire-mode evil doom-themes doom-modeline all-the-icons try paradox use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
