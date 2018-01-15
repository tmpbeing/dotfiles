; **************************************************************************** ;
;                                                                              ;
;                                                         :::      ::::::::    ;
;    init.el                                            :+:      :+:    :+:    ;
;                                                     +:+ +:+         +:+      ;
;    By: mplanell <mplanell@student.42.fr>          +#+  +:+       +#+         ;
;                                                 +#+#+#+#+#+   +#+            ;
;    Created: 2017/12/06 19:27:38 by mplanell          #+#    #+#              ;
;    Updated: 2018/01/16 00:27:38 by mplanell         ###   ########.fr        ;
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
(setq inhibit-startup-message t) ;Start emacs on a scratch buffer

;; Display
(menu-bar-mode 0)
(tool-bar-mode 0)
(blink-cursor-mode 0)
(scroll-bar-mode 0)
(tooltip-mode 0)

;; Tabs
(setq-default tab-width 4
	indent-tabs-mode t)

;; Files

;;; Backup
(setq backup-by-copying t
	delete-old-versions t
	kept-new-versions 4
	kept-old-versions 2
	version-control t
	backup-directory-alist `(("." . "~/.emacs.d/backups")))

;;; .custom.el
(setq-default custom-file (expand-file-name ".custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; Other
(setq sentence-end-double-space nil)

; Packages

; General : keybinding package
(use-package general
	:ensure t
	:config
	(general-define-key "C-'" 'avy-goto-word-1)
	)

; Avy : allows to move around in visible text (~= easymotion)
(use-package avy
	:ensure t
	:commands (avy-goto-word-1)
	)

; Which-key
(use-package which-key
	:ensure t
	)

; 42Header
(use-package header
	:load-path "./local/42header/"
	:config
	(general-define-key "C-c C-h" 'header-insert)
	)
