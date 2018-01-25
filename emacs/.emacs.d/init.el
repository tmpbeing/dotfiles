; **************************************************************************** ;
;                                                                              ;
;                                                         :::      ::::::::    ;
;    init.el                                            :+:      :+:    :+:    ;
;                                                     +:+ +:+         +:+      ;
;    By: mplanell <mplanell@student.42.fr>          +#+  +:+       +#+         ;
;                                                 +#+#+#+#+#+   +#+            ;
;    Created: 2017/12/06 19:27:38 by mplanell          #+#    #+#              ;
;    Updated: 2018/01/24 21:00:42 by mplanell         ###   ########.fr        ;
;                                                                              ;
; **************************************************************************** ;

; Init

(setq package-enable-at-startup nil)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))

(require 'use-package)

; Config

;; Start
(setq inhibit-startup-message t
	  initial-scratch-message "") ;Start emacs on a empty scratch buffer

;; Display
(menu-bar-mode 0)
(tool-bar-mode 0)
(blink-cursor-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)

;; Font
(set-default-font "PragmataPro for Powerline-12")

;; Tabs and whitespace
(setq-default tab-width 4
	indent-tabs-mode t)
(setq indent-line-function 'insert-tab)
(setq c-default-style "linux"
		c-basic-offset 4) ; Use 4 size tabs for C

;; Remove trailing whitespace on save. Do not remove trailing lines
(setq delete-trailing-lines nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Files

;;; Backup
(setq make-backup-files nil) ; Do not make backups
(setq backup-directory-alist `((".*" . ,temporary-file-directory))) ; Most likely useless

;;; Auto-save to tmp folder
(setq auto-save-file-name-transforms
`((".*" ,temporary-file-directory t)))

;;; .custom.el for emacs built-in customisation
(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Other

(setq sentence-end-double-space nil) ; Sentences end with a dot and a space
(defalias 'yes-or-no-p 'y-or-n-p) ; no more typing out y.e.s.

;;; utf-8 everywhere
(set-language-environment 'utf-8)
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-file-name-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

; Packages

;; Global

;;; 42Header
(use-package header
	:load-path "./local/42header/"
	)

;;; Avy : allows to move around in visible text (~= easymotion)
(use-package avy
	:ensure t
	:commands
	(avy-goto-word-1
	avy-goto-word-or-subword-1
	avy-goto-char-in-line
	avy-goto-line)
	)

;;; Column-enforce-mode (show lines past 80 characters)
(use-package column-enforce-mode
	:ensure t
	:diminish
	:config
	(setq column-enforce-column 80)
	(add-hook 'prog-mode-hook 'column-enforce-mode)
	)

;;; Counsel
(use-package counsel
	:after ivy
	:ensure t
	)

;;; Diminish for the :diminish support in use-package
(use-package diminish
	:ensure t
	)

;;; Doom-themes, best themes on the market
(use-package doom-themes
	:ensure t
	:config
	(load-theme 'doom-one t)
	)
(use-package solaire-mode
	:ensure t
	:after doom-themes
	:config
	(add-hook 'after-change-major-mode-hook #'turn-on-solaire-mode)
	(solaire-mode-swap-bg)
	)

;;; Ivy, the completion framework
(use-package ivy
	:ensure t
	:diminish (ivy-mode . "")
	:init
	(setq ivy-use-virtual-buffers t
			ivy-count-format "%d/%d "
			ivy-display-style 'fancy
			ivy-height 12)
	:config
	(ivy-mode 1)
	)

;;; Neotree, a nerdtree equivalent for emacs with all-the-icons for doom theme
(use-package neotree
	:ensure t
	:commands (neotree)
	:init
	(setq neo-smart-open t
		  neo-show-hidden-files t)
	(doom-themes-neotree-config)
	)
(use-package all-the-icons
	:ensure t
	)

;;; Powerline
(use-package powerline
	:ensure t
	:config
	(powerline-center-evil-theme)
	(setq powerline-default-separator nil
		  powerline-height 24)
	)
(use-package airline-themes
	:after powerline
	:ensure t
	:config
	(load-theme 'airline-doom-one t)
	(setq airline-shortened-directory-length 25
		airline-utf-glyph-linenumber #xe0a1
		airline-utf-glyph-readonly #xe0a2
		airline-utf-glyph-branch #xe0a0)
	)

;;; Swiper
(use-package swiper
	:after ivy
	:ensure t
	:diminish
	)

;;; Whitespace mode (this is built-in)
(use-package whitespace
	:diminish
	:config
	(add-hook 'prog-mode-hook 'whitespace-mode)
	(setq whitespace-style '(face tabs tab-mark))
	)

;;; Which-key
(use-package which-key
	:ensure t
	:diminish
	:init
	(setq which-key-idle-delay 0.2
		  which-key-popup-style 'side-window
		  which-key-side-window-location 'bottom
		  enable-recursive-minibuffers t)
	:config
	(which-key-mode 1)
	)

;; Language-specific

;;; Markdown
(use-package markdown-mode
	:ensure t
	:commands (markdown-mode)
	:delight markdown-mode "Markdown"
	:mode
	("INSTALL\\'"
	"CONTRIBUTORS\\'"
	"LICENSE\\'"
	"README\\'"
	"\\.markdown\\'"
	"\\.md\\'")
	)

(load-file "~/.emacs.d/keybindings.el")
(load-file "~/.emacs.d/org.el")
