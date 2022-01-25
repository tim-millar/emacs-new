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

(add-to-list 'load-path "~/.emacs.d/custom")

(use-package setup-utils)

(use-package gnu-elpa-keyring-update
  :ensure t)

(require 'server)
(unless (server-running-p)
 (server-start))

(use-package general
  :ensure t
  :init
  (setq general-override-states '(insert
                                  emacs
                                  hybrid
                                  normal
                                  visual
                                  motion
                                  operator
                                  replace))
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
   :keymaps 'override
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
   "hb" 'describe-bindings
   "hc" 'helpful-command
   "hd" 'counsel-descbinds
   "he" 'info-emacs-manual
   "hf" 'counsel-describe-function
   "hF" 'helpful-function
   "hh" 'apropos-documentation
   "hi" 'info
   "hk" 'describe-key
   "hl" 'counsel-find-library
   "hm" 'describe-mode
   "hp" 'describe-package
   "hs" 'describe-syntax
   "hv" 'counsel-describe-variable
   "hw" 'helpful-at-point

   "b" '(:ignore t :which-key "buffers")
   "bb" 'persp-ivy-switch-buffer
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
   "jx" 'xref-find-definitions

   "p" '(projectile-command-map :which-key "projectile")
   "r" '(projectile-rails-command-map :which-key "projectile-rails")
   "a" '(rspec-mode-keymap :which-key "rspec")
   "l" '(lsp-mode-map :which-key "lsp-mode")

   "xi" 'tm/iterm-focus
   "xd" 'tm/iterm-goto-filedir-or-home
   "xx" 'eshell-here
   "xb" 'ruby-toggle-block
   "xs" 'ruby-toggle-string-quotes
   "xy" 'ruby-tools-to-symbol
   "xu" 'sp-unwrap-sexp
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

(setq default-frame-alist '((font . "Hack 13")))

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
      '(("." . "~/.emacs.d/backups"))
      auto-save-file-name-transforms
      `((".*" "~/.emacs.d/auto-save-list/" t))
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

;; (use-package solaire-mode
;;   :ensure t
;;   :hook
;;   ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
;;   :config
;;   (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
;;   (solaire-mode-swap-bg))

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
        ("C-l" . ivy-alt-done)
				("C-j" . ivy-next-line)
				("C-k" . ivy-previous-line)
				("M-j" . ivy-next-history-element)
				("M-k" . ivy-previous-history-element)
        :map ivy-switch-buffer-map
        ("C-k" . ivy-previous-line)
        ("C-l" . ivy-done)
        ("C-d" . ivy-switch-buffer-kill)
        :map ivy-reverse-i-search-map
        ("C-k" . ivy-previous-line)
        ("C-d" . ivy-reverse-i-search-kill))
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
  :init
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  :config
  (ivy-rich-mode t))

(use-package ivy-xref
  :ensure t
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(use-package ivy-prescient
  :ensure t
  :after counsel
  :init
  (setq prescient-sort-length-enable nil
        ivy-prescient-retain-classic-highlighting t)
  :config
  (ivy-prescient-mode 1)
  (prescient-persist-mode 1))

(use-package company-prescient
  :ensure t
  :after company
  :config
  (company-prescient-mode 1)
  (prescient-persist-mode 1))

;; ==============================
;; Projectile & Perspective
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

;; (use-package perspective
;;   :ensure t
;;   :bind
;;   ("C-x C-b" . persp-list-buffers)   ; or use a nicer switcher, see below
;;   :init
;;   (setq persp-modestring-short t)
;;   :config
;;   (persp-mode))

;; (use-package persp-projectile
;;   :ensure t
;;   :after (projectile perspective))

(use-package helpful
  :ensure t
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

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

(use-package slim-mode
  :ensure t
  :mode
  (("\\.slim\\'" . slim-mode)))

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
        compilation-scroll-output t
        rspec-use-docker-when-possible t
        rspec-docker-command "docker-compose exec"
        rspec-docker-cwd "/usr/src/"
        rspec-docker-container "web")
  :config
  (rspec-install-snippets)
  :hook dired-mode)

(use-package rubocop
  :ensure t
  :config
  (add-hook 'enh-ruby-mode #'rubocop-mode))

(use-package rufo
  :ensure t
  :init
  (setq rufo-minor-mode-use-bundler t)
  :config
  (add-hook 'enh-ruby-mode 'rufo-minor-mode))

;; (use-package rubocopfmt
;;   :ensure t
;;   :config
;;   (add-hook 'enh-ruby-mode #'rubocopfmt-mode))


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

(use-package vterm
    :ensure t)

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
  (progn (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode)))
  (require 'dap-chrome)
  (dap-chrome-setup)
  :hook (rjsx-mode . lsp-deferred))

(use-package typescript-mode
  :ensure t
  :init
  (setq typescript-indent-level 2)
  :config
  (add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode))
  (require 'dap-chrome)
  (dap-chrome-setup)
  :hook
  (typescript-mode . lsp-deferred))

(use-package add-node-modules-path
  :ensure t
  :config
  (add-hook 'js-mode-hook 'add-node-modules-path)
  (add-hook 'jsx-mode-hook 'add-node-modules-path)
  (add-hook 'rjsx-mode-hook 'add-node-modules-path)
  (add-hook 'typescript-mode-hook 'add-node-modules-path)
  (add-hook 'js2-mode-hook 'add-node-modules-path))

(use-package prettier-js
  :ensure t
  :after add-node-modules-path
  :init
  :config
  (add-hook 'js-mode-hook 'prettier-js-mode)
  (add-hook 'jsx-mode-hook 'prettier-js-mode)
  (add-hook 'rjsx-mode-hook 'prettier-js-mode)
  (add-hook 'typescript-mode-hook 'prettier-js-mode)
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

;; (use-package hcl-mode
;;   :ensure t)

(use-package terraform-mode
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
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
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

(use-package graphql-mode
  :ensure t)

(use-package company
  :ensure t)

;; (use-package company
;;   :ensure t
;;   :init
;;   (global-company-mode)
;;   :config
;;   (setq company-tooltip-align-annotations t
;;         company-show-numbers t
;;         company-idle-delay 0
;;         company-dabbrev-downcase nil)
;;   :diminish company-mode)

;; (use-package company-quickhelp          ; Documentation popups for Company
;;   :ensure t
;;   :defer t
;;   :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode))

;; (use-package company-tabnine
;;   :ensure t
;;   :init
;;   (add-to-list 'company-backends #'company-tabnine))

;; ==============================
;; lsp
;; ==============================
;; install lsp for all of these??
;; (add-hook 'js-mode-hook 'prettier-js-mode)
;; (add-hook 'jsx-mode-hook 'prettier-js-mode)
;; (add-hook 'js2-mode-hook 'prettier-js-mode))

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (typescript-mode . lsp)
         (rjsx-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  )

(use-package lsp-ivy
  :ensure t
  :after lsp-mode
  :commands lsp-ivy-workspace-symbol)

(use-package dap-mode
  :ensure t
  :after lsp-mode
  :hook (('dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))))

;; ==============================
;; Elixir
;; ==============================

(use-package elixir-mode
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
;; Org
;; ==============================
;; (defun efs/org-mode-setup ()
;;   (org-indent-mode)
;;   (variable-pitch-mode nil)
;;   (auto-fill-mode 0)
;;   (visual-line-mode 1)
;;   (setq evil-auto-indent nil)
;;   (diminish org-indent-mode))

(use-package org
  :ensure t
  :pin org
  :diminish org
  :init
  (setq org-ellipsis " ▾"
        org-hide-emphasis-markers t)
  (bind-key "C-c c" 'org-capture)
  ;; :hook (org-mode . efs/org-mode-setup)
  )

(use-package org-bullets
  :ensure t
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

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

                              ;; (eshell/alias "ec" "find-file $1")
                              ;; (eshell/alias "ff" "find-file $1")
                              ;; (eshell/alias "fo" "find-file-other-window $1")
                              ;; (eshell/alias "emacs" "find-file $1")
                              ;; (eshell/alias "oo" "find-file-other-window $1")
                              ;; (eshell/alias "ll" "ls -AlohG")
                              ;; (eshell/alias "d" "dired $1")
                              ;; (eshell/alias "do" "dired-other-window $1")

                              ;; (eshell/alias "be" "bundle exec")
                              ;; (eshell/alias "befs" "bundle exec foreman start")

                              ;; (eshell/alias "gco" "git checkout $1")
                              ;; (eshell/alias "gbv" "git branch -vv")
                              ;; (eshell/alias "gfa" "git fetch --all")
                              ;; (eshell/alias "gfb" "gfa && gbv")

                              ;; (eshell/alias "gd" "magit-diff-unstaged")
                              ;; (eshell/alias "gds" "magit-diff-staged")
                              ;; (eshell/alias "gst" "magit-status")
                              ;; (eshell/alias "gl" "magit-log-current")
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

(set-register ?e (cons 'file "~/.emacs.d/init.el"))
(set-register ?z (cons 'file "~/.zshrc"))
(set-register ?n (cons 'file "~/Org/notes.org"))

;; ==============================
;; Evil
;; ==============================

(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (setq evil-disable-insert-state-bindings t)
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (evil-set-initial-state 'eshell-mode 'emacs)
  (evil-set-initial-state 'inf-ruby-mode 'emacs)
  (evil-set-initial-state 'commint-mode 'normal)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-mode 1))

(use-package evil-collection
  :ensure t
  :after evil
  :config
  (evil-collection-init))

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

(use-package undo-fu
  :ensure t
  :config
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map "\C-r" 'undo-fu-only-redo))

;; ==============================
;; Version Controll
;; ==============================

(use-package magit
  :ensure t
  :after ivy
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
	(magit-wip-mode 1))

(use-package git-timemachine
  :ensure t
  :config
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))

(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map ()
  (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

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
   '(undo-fu company-prescient ivy-prescient org-bullets helpful vterm dap-mode ivy-xref lsp-ivy lsp-mode persp-projectile perspective graphql-mode elixir-mode evil-collection :emmet-mode slim-mode typescript-mode hcl-mode terraform-mode company-tabnine company-quickhelp company company-mode gnu-elpa-keyring-update rufo exec-path-from-shell exec-path docker-tramp rubocopfmt ivy-rich ivy-rich-mode string-inflection rubocop ruby-tools markdown-mode elfeed dumb-jump rjsx-mode flycheck prettier-js add-node-modules-path git-timemachine emmet-mode dockerfile-mode react-snippets evil-surround ibuffer-vc yasnippet-snippets eshell-git-prompt yasnippet robe bundler rspec-mode web-mode rvm enh-ruby-mode projectile-rails counsel-projectile evil-nerd-commenter projectile all-the-icons-ivy all-the-icons-dired evil-indent-plus evil-textobj-anyblock counsel swiper ivy-hydra evil-smartparens smartparens-config smartparens ivy elisp-slime-nav general evil-escape evil-magit magit solaire-mode evil doom-themes doom-modeline all-the-icons try paradox use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
