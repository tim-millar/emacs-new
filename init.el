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

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))
  (exec-path-from-shell-copy-env "HOME/.nvm"))

(require 'use-package)
(require 'json)

(add-to-list 'load-path "~/.emacs.d.new/custom")


(use-package setup-utils)

(use-package general
  :ensure t
  :config

  ;; (general-create-definer tyrant-def
  ;;  :states '(normal visual insert emacs)
  ;;  :prefix "SPC"
  ;;  :non-normal-prefix "C-SPC"
  ;;  )

  ;; (tyrant-def
  ;;   ""     nil
  ;;   "c"   (general-simulate-key "C-c")
  ;;   "h"   (general-simulate-key "C-h")
  ;;   "u"   (general-simulate-key "C-u")
  ;;   "x"   (general-simulate-key "C-x")
  ;;  )

  (general-define-key
   :keymaps 'global-map
   :states '(normal visual insert emacs)
   :prefix "SPC"
   :non-normal-prefix "C-SPC"

   "SPC" '(counsel-M-x :which-key "M-x")
   "TAB" 'next-buffer
   "DEL" 'previous-buffer

   ;; "," 'evilnc-comment-operator
   ;; "." 'evilnc-copy-and-comment-operator

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
	 "gf" 'magit-log-buffer-file
   "gs" 'magit-status
   "gg" 'counsel-git-grep
   "gt" 'git-timemachine-toggle
   "gb" 'magit-blame

   "j"  '(:ignore t :which-key "jump")
   "jj" 'dumb-jump-go

   "p" '(projectile-command-map :which-key "projectile")
   "r" '(projectile-rails-command-map :which-key "projectile-rails")
   "a" '(rspec-mode-keymap :which-key "rspec")

   "xi" 'tm/iterm-focus
   "xd" 'tm/iterm-goto-filedir-or-home
   "xx" 'eshell-here
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
(winner-mode t)

(setq column-number-mode t
      sentence-end-double-space nil
      fill-column 80
      ispell-program-name "aspell")

(defalias 'list-buffers 'ibuffer)

(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'kill-emacs-query-functions
	  (lambda () (y-or-n-p "Do you really want to exit Emacs? "))
	  'append)

(defun tm/setup-line-numbers ()
  "Setup line numbers."
  (progn
    (display-line-numbers-mode t)
    (setq display-line-numbers-type 'relative
          display-line-numbers-current-absolute t
          display-line-numbers-width 3)))

(add-hook 'prog-mode-hook 'tm/setup-line-numbers)

;; file backups
(setq backup-directory-alist
      '(("." . "~/.emacs.d.new/backups"))
      auto-save-file-name-transforms
      `((".*" "~/.emacs.d.new/auto-save-list/" t))
      backup-by-copying t)

;; themes, modeline and icons

(defun tm/switch-theme (theme)
  "This interactive call is taken from `load-theme'. THEME is the new theme."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapc 'symbol-name
                                     (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
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
	(setq enable-recursive-minibuffers t
				ivy-use-virtual-buffers t
				ivy-virtusl-abbreviate "full"
				ivy-height 12
				ivy-wrap t
				ivy-use-selectable-prompt t
				ivy-count-format "(%d/%d) ")
  :bind
	(:map ivy-minibuffer-map
				("C-;" . ivy-avy)
				("C-j" . ivy-next-line)
				("C-k" . ivy-previous-line)
				("M-j" . ivy-next-history-element)
				("M-k" . ivy-previous-history-element))
	:config
	(ivy-mode t)
	(ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur)
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

(use-package ivy-rich
  :ensure t
  :after ivy
  :config
  (ivy-rich-mode t))

;; ==============================
;; Projectile
;; ==============================

(use-package projectile
  :ensure t
  :init
  (setq projectile-switch-project-action 'projectile-dired)
  (setq projectile-completion-system 'ivy)
  (setq projectile-mode-line ;; ???
	'(:eval (format " [%s]" (projectile-project-name))))
  :config
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
  :init
  (setq ruby-insert-encoding-magic-comment nil)
  (setq enh-ruby-add-encoding-comment-on-save nil)
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
  :config
  (add-hook 'enh-ruby-mode-hook 'robe-mode))

(use-package ruby-tools
  :ensure t)

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
  :config
  (rspec-install-snippets)
  :hook dired-mode)

(use-package rubocop
  :ensure t
  :config
  (add-hook 'enh-ruby-mode #'rubocop-mode))

(use-package rubocopfmt
  :ensure t
  :config
  (add-hook 'enh-ruby-mode #'rubocopfmt-mode))

;; ==============================
;; Programming
;; ==============================

(setq-default c-basic-offset 2
              indent-tabs-mode nil
							tab-width 2
							js-indent-level 2
							coffee-tab-width 2
							javascript-indent-level 2
							js2-basic-offset 2
              js2-mode-show-strict-warnings nil
							web-mode-css-indent-offset 2
							web-mode-markup-indent-offset 2
							web-mode-code-indent-offset 2
							css-indent-offset 2)

(use-package dumb-jump
  :ensure t
  :defer t
  :init
  (setq dumb-jump-selector 'ivy)
  (setq dumb-jump-aggressive nil)
  :config
  (dumb-jump-mode))

(use-package rjsx-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode)))

(use-package add-node-modules-path
  :ensure t
  :config
  (add-hook 'js-mode-hook 'add-node-modules-path)
  (add-hook 'jsx-mode-hook 'add-node-modules-path)
  (add-hook 'rjsx-mode-hook 'add-node-modules-path)
  (add-hook 'js2-mode-hook 'add-node-modules-path))

(use-package prettier-js
  :ensure t
  :after add-node-modules-path
  :config
  (add-hook 'js-mode-hook 'prettier-js-mode)
  (add-hook 'jsx-mode-hook 'prettier-js-mode)
  (add-hook 'rjsx-mode-hook 'prettier-js-mode)
  (add-hook 'js2-mode-hook 'prettier-js-mode))

(defun set-jsx-indentation ()
  "Set indentation for jsx mode."
  (setq-local sgml-basic-offset js-indent-level))

(add-hook 'js-jsx-mode-hook #'set-jsx-indentation)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

(use-package react-snippets
	:ensure t
  :after yasnippet)

(use-package dockerfile-mode
	:ensure t)

(use-package docker-tramp
  :ensure t)

(use-package emmet-mode
	:ensure t
	:config
	(setq emmet-expand-jsx-className? t)
	:init
	(add-hook 'sgml-mode-hook 'emmet-mode)
	(add-hook 'css-mode-hook 'emmet-mode))

(use-package flycheck
  :ensure t
  :after add-node-modules-path
  :init
  (global-flycheck-mode)
  (flycheck-add-mode 'javascript-eslint 'js-mode)
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
  (flycheck-add-mode 'javascript-eslint 'jsx-mode))

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package string-inflection
  :ensure t
  :bind
  (("C-c i" . string-inflection-all-cycle)))

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
;; Org
;; ==============================

(use-package org
  :ensure t
  :pin org
  :init
  (setq org-hide-emphasis-markers t)
  (bind-key "C-c c" 'org-capture))

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

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the current
buffer's file. The eshell is renamed to match that directory to make
multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert (concat "ls"))
    (eshell-send-input)))

(defun eshell/cdp ()
  "Change directory to the project's root."
  (eshell/cd (projectile-project-root)))

;; (defun eshell/d (&rest args)
;;   "Opens ARGS in dired mode."
;;   (dired (pop args) "."))

(defun tm/eshell-quit-or-delete-char (arg)
  "Quits the eshell or deletes ARG (a char)."
  (interactive "p")
  (if (and (eolp) (looking-back eshell-prompt-regexp))
      (progn
        (eshell-life-is-too-much) ; Why not? (eshell/exit)
        (delete-window))
    (delete-forward-char arg)))

;; define aliases
(add-hook 'eshell-mode-hook (lambda ()

                              (eshell/alias "ec" "find-file $1")
                              (eshell/alias "ff" "find-file $1")
                              (eshell/alias "fo" "find-file-other-window $1")
                              (eshell/alias "emacs" "find-file $1")
                              (eshell/alias "oo" "find-file-other-window $1")
                              (eshell/alias "ll" "ls -AlohG")
                              (eshell/alias "d" "dired $1")
                              (eshell/alias "do" "dired-other-window $1")

                              (eshell/alias "be" "bundle exec")
                              (eshell/alias "befs" "bundle exec foreman start")

                              (eshell/alias "gco" "git checkout $1")
                              (eshell/alias "gbv" "git branch -vv")
                              (eshell/alias "gfa" "git fetch --all")
                              (eshell/alias "gfb" "gfa && gbv")

                              (eshell/alias "gd" "magit-dff-unstaged")
                              (eshell/alias "gds" "magit-diff-staged")
                              (eshell/alias "gst" "magit-status")
                              (eshell/alias "gl" "magit-log-current")
                              (define-key eshell-mode-map (kbd "C-d")
                                'tm/eshell-quit-or-delete-char)))

(use-package ibuffer-vc
	:ensure t
	:after evil
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

(set-register ?e (cons 'file "~/.emacs.d.new/init.el"))
(set-register ?z (cons 'file "~/.zshrc"))
(set-register ?n (cons 'file "~/Org/notes.org"))

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
  (evilnc-default-hotkeys))

(use-package evil-surround
	:ensure t
	:after evil
	:config
	(global-evil-surround-mode))

;; ==============================
;; Version Controll
;; ==============================

(use-package magit
  :ensure t
  :after ivy
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
	(magit-wip-mode 1))

(use-package evil-magit
  :ensure t
  :after (evil magit)
  :config
  (setq evil-magit-state 'normal))

(use-package git-timemachine
  :ensure t
  :config
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

;; ==============================
;; Packages
;; ==============================

(use-package elfeed
  :ensure t
  :bind
  (("C-x w" . elfeed))
  :init
  (setq elfeed-feeds
        '(("http://newmonetarism.blogspot.com/feeds/posts/default" econ))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (exec-path-from-shell exec-path docker-tramp rubocopfmt ivy-rich ivy-rich-mode string-inflection rubocop ruby-tools markdown-mode elfeed dumb-jump rjsx-mode flycheck prettier-js add-node-modules-path git-timemachine emmet-mode dockerfile-mode react-snippets evil-surround ibuffer-vc yasnippet-snippets eshell-git-prompt yasnippet robe bundler rspec-mode web-mode rvm enh-ruby-mode projectile-rails counsel-projectile evil-nerd-commenter projectile all-the-icons-ivy all-the-icons-dired evil-indent-plus evil-textobj-anyblock counsel swiper ivy-hydra evil-smartparens smartparens-config smartparens ivy elisp-slime-nav general evil-escape evil-magit magit solaire-mode evil doom-themes doom-modeline all-the-icons try paradox use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
