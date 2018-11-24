;; load package and set archives
(require 'package)

(setq package-enable-at-startup nil)

(setq package-archives '(
			 ("melpa" . "https://melpa.org/packages/")
			 ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("gnu" . "https://elpa.gnu.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 )
      package-archive-priorities '(("melpa" . 1)))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(require 'json)

(add-to-list 'load-path "~/.emacs.d.new/custom")

(use-package setup-utils)

(use-package general
  :ensure t
  :config
  (general-define-key
   :keymaps 'global-map
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

   "SPC" '(counsel-M-x :which-key "M-x")
   "TAB" 'next-buffer
   "DEL" 'previous-buffer

   "," 'evilnc-comment-operator
   "." 'evilnc-copy-and-comment-operator

   ;; "h" '(help-map :which-key "help")
   "h" '(:ignore t :which-key "help")
   "ha" 'counsel-apropos
   "hh" 'apropos-documentation
   "hi" 'info
   "hk" 'describe-key
   "hd" 'counsel-descbinds
   "hf" 'counsel-describe-function
   "hv" 'counsel-describe-variable
   "hb" 'describe-bindings
   "hm" 'describe-mode
   "hp" 'describe-package
   "he" 'info-emacs-manual
   "hs" 'describe-syntax
   "hl" 'counsel-find-library

   "b" '(:ignore t :which-key "buffers")
   "bb" 'ivy-switch-buffer
   "bi" 'ibuffer
   "bk" 'kill-this-buffer
   "bs" 'save-buffer
   "bS" 'save-some-buffer
   "b-" 'split-window-vertically
   "b/" 'split-window-horizontally
	 "bd" 'dired-jump

   "c" '(:ignore t :which-key "counsel")
   "cc" '(ivy-resume :which-key "resume")
   "cs" '(swiper :which-key "swiper")
   "cm" '(swiper-multi :which-key "multi")
   "ca" '(counsel-ag :which-key "ag")
   "ci" '(counsel-imenu :which-key "imenu")
   "ct" '(counsel-load-theme :which-key "themes")
   "cy" '(counsel-yank-pop :which-key "yank-pop")
   "cl" '(counsel-locate :which-key "locate")
   "cw" '(counsel-colors-web :which-key "colors-web")
   "cf" '(counsel-fzf :which-key "fzf")
   "cr" '(counsel-evil-registers :which-key "evil-registers")
   "cp" '(counsel-package :which-key "packages")
   "cg" '(counsel-rg :which-key "rip-grep")
   "cd" '(counsel-dired-jump :which-key "dired-jump")
   "cj" '(counsel-file-jump :which-key "file-jump")
   "cf" '(counsel-recentf :which-key "recentf")

   "f" '(:ignore t :which-key "files")
   "ff" 'counsel-find-file

   "g" '(:ignore t :which-key "git")
   "gs" 'magit-status
   "gg" 'counsel-git-grep

   "p" '(projectile-command-map :which-key "projectile")
   "r" '(projectile-rails-command-map :which-key "projectile-rails")
   "a" '(rspec-mode-keymap :which-key "rspec")

   "xi" 'tm/iterm-focus
   "xd" 'tm/iterm-goto-filedir-or-home
	 ))

(use-package paradox
  :ensure t
  :diminish (paradox-menu-mode . "Paradox")
  :config
  (paradox-enable))

(use-package try
  :ensure t
  :commands try)

(use-package elisp-slime-nav
  :ensure t
  :hook
  ((emacs-lisp-mode . elisp-slime-nav-mode)
   (ielm-mode . elisp-slime-nav-mode)))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode)

(use-package smartparens-config
  :after smartparens
  :init
  (smartparens-global-mode t)
  (smartparens-global-strict-mode t)
  (show-smartparens-global-mode t))

(use-package evil-smartparens
  :ensure t
  :diminish evil-smartparen-mode
  :after (evil smartparens)
  :init
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

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
  :init
  (setq all-the-icons-color-icons t))

(use-package all-the-icons-dired
  :ensure t
  :diminish all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package all-the-icons-ivy
  :ensure t
  :after (all-the-icons ivy counse)
  :config
  (all-the-icons-ivy-setup))

(use-package doom-themes
  :ensure t
  :config
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

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :init
  (ivy-mode t)
  :bind
  (:map ivy-minibuffer-map
	("C-;" . ivy-avy)
	("C-j" . ivy-next-line)
	("C-k" . ivy-previous-line)
	("M-j" . ivy-next-history-element)
	("M-k" . ivy-previous-history-element))
  :config
  (setq enable-recursive-minibuffers t
	ivy-use-virtual-buffers t
	ivy-virtusl-abbreviate "full"
	ivy-height 12
	ivy-wrap t
	ivy-count-format "(%d/%d) ")
  (ivy-set-occur 'counsel-git-grep 'counsel-git-grep-occur)
  (ivy-set-occur 'swiper 'swiper-occur))

(use-package hydra
  :ensure t)

(use-package ivy-hydra
  :ensure t
  :after (ivy hydra))

(use-package swiper
  :ensure t)

(use-package counsel
  :ensure t)

;; ==============================
;; Projectile
;; ==============================

(use-package projectile
  :ensure t
  :config
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-completion-system 'ivy)
  (setq projectile-mode-line
	'(:eval (format " [%s]" (projectile-project-name))))
  (projectile-mode t))

(use-package counsel-projectile
  :ensure t
  :after (projectile counsel)
  :config
  (counsel-projectile-mode))

;; ==============================
;; Rails IDE
;; ==============================

(use-package projectile-rails
  :ensure t
  :diminish projectile-rails-mode
  :after projectile
  :init
  (setq projectile-rails-vanilla-command "bin/rails")
  :config
  (projectile-rails-global-mode)
  (general-define-key
   :keymaps 'projectile-rails-command-map
   "M-o" 'hydra-projectile-rails/body))

(use-package enh-ruby-mode
  :ensure t
  :mode
  ("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode)
  :interpreter "ruby")

(use-package inf-ruby
  :ensure t
  :after enh-ruby-mode)

(use-package rvm
  :ensure t
  :config
  (rvm-use-default))

(use-package web-mode
  :ensure t
  :mode
  (("\\.erb$" . web-mode)
   ("\\.html?" . web-mode))
  :init
  (setq web-mode-code-indent-offset 2
	web-mode-css-indent-offset 2
	web-mode-enable-css-colorization t
	web-mode-markup-indent-offset 2
	web-mode-script-padding 2
	web-mode-style-padding 2))

(use-package robe
  :ensure t
  :after enh-ruby-mode
  :diminish robe-mode
  :hook '(enh-ruby-mode))

(use-package yaml-mode
  :ensure t)

(use-package bundler
  :ensure t)

(use-package rspec-mode
  :ensure t
  :diminish rspec-mode
  :init
  (setq rspec-use-rake-when-possible nil
	compilation-scroll-output t)
  :hook dired-mode)

;; ==============================
;; Programming
;; ==============================

(setq-default c-basic-offset 2
	      tab-width 2
	      js-indent-level 2
	      coffee-tab-width 2
	      javascript-indent-level 2
	      js2-basic-offset 2
	      web-mode-css-indent-offset 2
	      web-mode-markup-indent-offset 2
	      web-mode-code-indent-offset 2
	      css-indent-offset 2)

(defun set-jsx-indentation ()
  (setq-local sgml-basic-offset js-indent-level))
(add-hook 'js-jsx-mode-hook #'set-jsx-indentation)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

;; ==============================
;; Dired
;; ==============================

(setq dired-recursive-copies 'always
      dired-recursive-deletes 'top
      dired-dwim-target t
      dired-use-ls-dired nil)

(add-hook 'dired-mode-hook
	  (lambda ()
	    (dired-hide-details-mode t)))

;; ==============================
;; Utilities
;; ==============================

(use-package eshell
  :init
  (setenv "PAGER" "cat")
  (setq eshell-visual-commands
	'("less" "tmux" "htop" "top" "bash" "zsh" "fish" "ssh" "tail" "vi")
	eshell-visual-subcommands
	'("git" ("log" "diff" "show"))))

(use-package eshell-git-prompt
  :ensure t
  :config
  (eshell-git-prompt-use-theme 'powerline))

(use-package ibuffer-vc
	:ensure t
	:init
	(setq ibuffer-formats
				'((mark modified read-only vc-status-mini " "
								(name 18 18 :left :elide)
								" "
								(size 9 -1 :right)
								" "
								(mode 16 16 :left :elide)
								" "
								(vc-status 16 16 :left)
								" "
								filename-and-process))
				evil-emacs-state-modes
				(delq 'ibuffer-mode evil-emacs-state-modes))
	:config
	(add-hook 'ibuffer-hook
						(lambda ()
							(ibuffer-auto-mode 1)
							(add-to-list 'ibuffer-never-show-predicates "^\\*projectile")
							(add-to-list 'ibuffer-never-show-predicates "^\\magit")
							(setq ibuffer-show-empty-filter-groups nil)
							(ibuffer-vc-set-filter-groups-by-vc-root)
							(unless (eq ibuffer-sorting-mode 'alphabetic)
								(ibuffer-do-sort-by-alphabetic)))))

;; ==============================
;; Evil
;; ==============================

(use-package evil
  :ensure t
  :init
  (setq evil-disable-insert-state-bindings t)
  (setq undo-tree-enable-undo-in-region nil)
  :config
  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'inf-ruby-mode 'emacs)
  (evil-set-initial-state 'commint-mode 'normal)
  (evil-mode 1))

(use-package evil-escape
  :ensure t
  :diminish evil-escape-mode
  :config
  (evil-escape-mode 1))

(use-package evil-textobj-anyblock
  :ensure t
  :config
  (define-key evil-inner-text-objects-map "b" 'evil-textobj-anyblock-inner-block)
  (define-key evil-outer-text-objects-map "b" 'evil-textobj-anyblock-a-block))

(use-package evil-indent-plus
  :ensure t
  :init
  (evil-indent-plus-default-bindings))

(use-package evil-nerd-commenter
  :ensure t
  :after evil
  :config
  (evilnc-default-hotkeys t))

;; ==============================
;; Version Controll
;; ==============================

(use-package magit
  :ensure t
  :after ivy
  :config
  (setq magit-completing-read-function 'ivy-completing-read))

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
		(ibuffer-vc yasnippet-snippets eshell-git-prompt yasnippet robe bundler rspec-mode web-mode rvm enh-ruby-mode projectile-rails counsel-projectile evil-nerd-commenter projectile all-the-icons-ivy all-the-icons-dired evil-indent-plus evil-textobj-anyblock counsel swiper ivy-hydra evil-smartparens smartparens-config smartparens ivy elisp-slime-nav general evil-escape evil-magit magit solaire-mode evil doom-themes doom-modeline all-the-icons try paradox use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
