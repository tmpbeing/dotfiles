; **************************************************************************** ;
;                                                                              ;
;                                                         :::      ::::::::    ;
;    init.el                                            :+:      :+:    :+:    ;
;                                                     +:+ +:+         +:+      ;
;    By: mplanell <mplanell@student.42.fr>          +#+  +:+       +#+         ;
;                                                 +#+#+#+#+#+   +#+            ;
;    Created: 2017/12/06 19:27:38 by mplanell          #+#    #+#              ;
;    Updated: 2018/01/27 14:29:10 by mplanell         ###   ########.fr        ;
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
	:hook
	(prog-mode . column-enforce-mode)
	:config
	(setq column-enforce-column 80)
	)

;;; Counsel
(use-package counsel
	:after ivy
	:ensure t
	:config
	(setq counsel-find-file-at-point t)
	)

;;; Diff-hl (~= git-gutter)
(use-package diff-hl
	:ensure t
	:hook
	(((prog-mode vc-dir-mode) . turn-on-diff-hl-mode)
	(dired-mode . diff-hl-dired-mode-unless-remote))
	)

;;; Diminish for the :diminish support in use-package
(use-package diminish
	:ensure t
	)

;;; Dired
(use-package dired
	:defer t
	:hook
	(dired-mode . dired-hide-details-mode)
	:init
	(setq dired-auto-revert-buffer t
		  auto-revert-verbose nil)
	)

;;; Dired-subtree
(use-package dired-subtree
	:ensure t
	:after dired)

;;; Doom-themes, best themes on the market with some custom faces
(use-package doom-themes
	:ensure t
	:config
	(load-theme 'doom-one t)
	(custom-theme-set-faces
		'doom-one
		`(org-level-1 ((t (:height 1.0 :weight bold   :slant normal :foreground "#aa88ff" :background "#21272d" :underline nil :box (:line-width 4 :color "#21272d" :style nil)))))
		`(org-level-2 ((t (:height 1.0 :weight bold   :slant normal :foreground "#88aaff" :background "#21272d" :underline nil :box (:line-width 4 :color "#21272d" :style nil)))))
		`(org-level-3 ((t (:height 1.0 :weight normal :slant normal :foreground "#88ffff" :underline nil)))) ;; cyan
		`(org-level-4 ((t (:height 1.0 :weight normal :slant normal :foreground "#66ffaa" :underline nil)))) ;; sea-green
		`(org-level-5 ((t (:height 1.0 :weight normal :slant normal :foreground "#ffff66" :underline nil)))) ;; yellow
		`(org-level-6 ((t (:height 1.0 :weight normal :slant normal :foreground "#ffaa00" :underline nil)))) ;; orange
		`(org-level-7 ((t (:height 1.0 :weight normal :slant normal :foreground "#ff6666" :underline nil)))) ;; red
		`(org-level-8 ((t (:height 1.0 :weight normal :slant normal :foreground "#ff66aa" :underline nil)))) ;; pink
		`(avy-lead-face ((t (:foreground "#ffaf00"))))
		`(avy-lead-face-0 ((t (:foreground "#5fd7ff"))))
		`(avy-lead-face-1 ((t (:foreground "#66ffaa"))))
		`(avy-lead-face-2 ((t (:foreground "#ff6666")))))
	)
(use-package solaire-mode
	:ensure t
	:after doom-themes
	:hook
	(after-change-major-mode . turn-on-solaire-mode)
	:config
	(solaire-mode-swap-bg)
	)

;;; Evil, vim in emacs
(use-package evil
	:ensure t
	:init
	(setq evil-want-integration nil
		  evil-move-cursor-back nil)
	:config
	(evil-mode 1)
	)

;;; Evil-collection, a set of keybindings for lots of plugins (gonna make my own bindings instead)
; (use-package evil-collection
	; :ensure t
	; :after evil
	; :config
	; (evil-collection-init)
	; )

;;; Evil-matchit, jump to matched tag with %
(use-package evil-matchit
	:ensure t
	:after evil
	:config
	(global-evil-matchit-mode 1)
	)

;;; Evil-nerd-commenter, port of vim nerd-comment
(use-package evil-nerd-commenter
	:ensure t
	:commands
	(evilnc-comment-or-uncomment-lines
		evilnc-comment-or-uncomment-paragraphs)
	)

;;; Evil-surround, port of vim-surround
(use-package evil-surround
	:ensure t
	:after evil
	:config
	(global-evil-surround-mode 1)
	)

;;; Evil-visualstar (* and # to search selection forward/backward)
(use-package evil-visualstar
	:ensure t
	:after evil
	:config
	(global-evil-visualstar-mode 1)
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
	(eval-after-load "ivy"
		`(progn
			(define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)))
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

;;; nlinum-relative for relative line numbering
(use-package nlinum-relative
	:ensure t
	:hook
	(prog-mode . nlinum-relative-mode)
	:config
	(setq nlinum-relative-current-symbol "")
	(nlinum-relative-setup-evil)
	)

;;; Powerline
(use-package powerline
	:ensure t
	:config
	(powerline-center-evil-theme)
	(setq powerline-default-separator nil
		  powerline-height 30)
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

;;; Recentf
(use-package recentf
	:init
	(setq recentf-max-menu-items 25)
	:config
	(recentf-mode 1)
	)

;;; Savehist (save minibuffer history)
(use-package savehist
  :init
  (setq savehist-autosave-interval 150)
  :config
  (savehist-mode)
  )

;;; Saveplace, save last position in the file
(use-package saveplace
	:config
	(save-place-mode)
	)

;;; Swiper
(use-package swiper
	:after ivy
	:ensure t
	:diminish
	)

;;; Undo-tree (required by evil)
(use-package undo-tree
	:ensure t
	:diminish
	:init
	(setq undo-tree-visualizer-timestamps t
		undo-tree-visualizer-diff t)
	)

;;; Whitespace mode (this is built-in)
(use-package whitespace
	:diminish
	:hook
	(prog-mode . whitespace-mode)
	:config
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
