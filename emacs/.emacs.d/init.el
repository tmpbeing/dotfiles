; **************************************************************************** ;
;                                                                              ;
;                                                         :::      ::::::::    ;
;    init.el                                            :+:      :+:    :+:    ;
;                                                     +:+ +:+         +:+      ;
;    By: mplanell <mplanell@student.42.fr>          +#+  +:+       +#+         ;
;                                                 +#+#+#+#+#+   +#+            ;
;    Created: 2017/12/06 19:27:38 by mplanell          #+#    #+#              ;
;    Updated: 2018/02/02 17:54:11 by mplanell         ###   ########.fr        ;
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
;; (set-default-font "PragmataPro for Powerline-12")
(set-frame-font "PragmataPro for Powerline-12")

;; Tabs and whitespace
(setq-default tab-width 4
    indent-tabs-mode nil)

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
(show-paren-mode) ; show matching parenthesis
(winner-mode) ; undo and redo window changes

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

;;; ace-window
(use-package ace-window
    :ensure t
    :init
    (setq aw-dispatch-always t
            aw-swap-invert t)
    :config
    (set-face-attribute 'aw-leading-char-face nil :foreground "deep sky blue" :weight 'bold :height 3.0)
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

;;; CC-mode
; (use-package cc-mode
  ; :config
  ; (add-hook 'cc-mode-hook 'c-mode-42-indentation-hook)
  ; (defun c-mode-42-indentation-hook ()
  ; (setq-default c-default-style "linux"
                ; c-basic-offset 4 ; Use 4 size tabs for C
                ; indent-tabs-mode t
                ; indent-line-function 'insert-tab))
  ; )


;;; Column-enforce-mode (show lines past 80 characters)
(use-package column-enforce-mode
    :ensure t
    :diminish
    :hook
    (prog-mode . column-enforce-mode)
    :config
    (setq column-enforce-column 80)
    )

;;; Company for auto-completion
(use-package company
    :ensure t
    :diminish
    :init
    (setq company-idle-delay 0.2
          company-tooltip-limit 15
          company-minimum-prefix-length 2
          company-dabbrev-downcase nil
          company-dabbrev-ignore-case nil)
	(company-tng-configure-default)
    :config
    (global-company-mode)
    (add-hook 'evil-insert-state-exit-hook 'company-abort)
    (setq company-backends
        '((company-files
            company-keywords
            company-capf
            company-clang
            company-gtags
            ;company-yasnippet
            )
        (company-abbrev company-dabbrev)
    ))
    ; (dolist (hook '(c-mode-hook
                    ; c++-mode-hook
                    ; objc-mode-hook
                    ; ))
        ; (add-hook hook
            ; (lambda()
                ; (make-local-variable 'company-backends)
                ; (setq company-backends (copy-tree company-backends))
                ; (setf (car company-backends)
                    ; (append '(company-gtags company-clang)
                    ; (car company-backends)))
            ; ))
    ; )
    )
(use-package company-c-headers
    :ensure t
    :after company
    :config
    (add-to-list 'company-backends 'company-c-headers)
    (setq company-c-headers-path-user '("." ".." "../include" "../includes" "../../include" "../../includes" "../../../include" "../../../includes" "./include" "./includes" "./libft/include" "./libft/includes" "../libft/include" "../libft/includes" "../../libft/include" "../../libft/includes" "../../../libft/include" "../../../libft/includes"))
    )

;;; Company quickhelp (tooltip documentation)
(use-package company-quickhelp
    :ensure t
    :after pos-tip
    :init
    (set-face-attribute 'tooltip nil :background "#303030" :foreground "#c6c6c6")
    :config
    (company-quickhelp-mode)
    )
(use-package pos-tip
    :ensure t
    )

;;; Counsel
(use-package counsel
    :after ivy
    :ensure t
    :config
    (setq counsel-find-file-at-point t)
    )

;; Desktop, save window arrangement
(use-package desktop
  :init
  (setq desktop-auto-save-timeout 30
        desktop-path '("~/.emacs.d/desktops")
        desktop-dirname "~/.emacs.d/desktops"
        desktop-base-file-name "base")
  :config
  (desktop-save-mode)
  )

;;; Diff-hl (~= git-gutter)
(use-package diff-hl
    :ensure t
    :hook
    (((prog-mode vc-dir-mode) . turn-on-diff-hl-mode)
    (dired-mode . diff-hl-dired-mode-unless-remote))
    :config
    (setq diff-hl-margin-mode t
          diff-hl-side 'right)
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
    :after dired
    )

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
        `(dired-subtree-depth-1-face ((t (:background "#23272e"))))
        `(dired-subtree-depth-2-face ((t (:background "#363d47"))))
        `(dired-subtree-depth-3-face ((t (:background "#4a5261"))))
        `(dired-subtree-depth-4-face ((t (:background "#5d687a"))))
        `(dired-subtree-depth-5-face ((t (:background "#717d94"))))
        `(dired-subtree-depth-6-face ((t (:background "#8493ad"))))
        '(flyspell-duplicate ((t (:underline "yellow" :weight bold))))
        '(flyspell-incorrect ((t (:underline "yellow" :weight bold))))
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

;;; Flycheck
(use-package flycheck
    :ensure t
    :diminish
    :config
    (setq flycheck-clang-args "-Wall -Wextra -Werror")
    (global-flycheck-mode)
    )

;;; Hydra
(use-package hydra
    :ensure t
    )

;;; hl-line
(use-package hl-line
  :config
  (global-hl-line-mode)
  )

;;; Ibuffer (advanced buffer menu)
(use-package ibuffer
    :init
    (setq ibuffer-use-header-line t
          ibuffer-use-other-window t)
    (add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))
    )
(use-package ibuffer-vc
    :ensure t
    :init
    (add-hook 'ibuffer-hook
    (lambda ()
        (ibuffer-vc-set-filter-groups-by-vc-root)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic))))
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

;;; Magit, version-control
(use-package magit
    :ensure t
    :defer t
    )
(use-package evil-magit
    :ensure t
    :after magit evil
    )
(use-package ediff
    :ensure t
    :init
    (setq ediff-split-window-function 'split-window-horizontally)
    )
(use-package evil-ediff
    :ensure t
    )

;;; Neotree, a nerdtree equivalent for emacs with all-the-icons for doom theme
(use-package neotree
    :ensure t
    :commands (neotree)
    :init
    (setq neo-smart-open t
          neo-show-hidden-files t)
    (doom-themes-neotree-config)
    (add-to-list 'evil-emacs-state-modes 'neotree-mode)
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
    (setq airline-shortened-directory-length 20
        airline-utf-glyph-linenumber #xe0a1
        airline-utf-glyph-readonly #xe0a2
        airline-utf-glyph-branch #xe0a0)
    )
; (use-package doom-modeline
  ; :defer t
  ; :load-path "~/.emacs.d/local"
  ; :preface
  ; (defun load-doom-modeline (frame)
    ; (select-frame frame)
    ; (require 'doom-modeline)
    ; (remove-hook 'after-make-frame-functions 'load-doom-modeline))
  ; :init
  ; (if (daemonp)
      ; (add-hook 'after-make-frame-functions #'load-doom-modeline)
    ; (require 'doom-modeline))

  ; (with-eval-after-load 'doom-modeline
    ; (defadvice doom-buffer-path (around ignore-remote first activate)
      ; (if (file-remote-p default-directory)
          ; (if buffer-file-name
              ; (setq ad-return-value (file-name-nondirectory (buffer-file-name)))
            ; (setq ad-return-value "%b"))
        ; ad-do-it)))

  ; (unless (file-exists-p "~/.emacs.d/local/doom-modeline.elc")
        ; (byte-compile-file "~/.emacs.d/local/doom-modeline.el")))

;;; Origami (folding with {{{ }}})
(use-package origami
  :ensure t
  :after dash
  )
(use-package dash
  :ensure t)

;;; Projectile (project browser)
(use-package projectile
    :ensure t
    :commands (projectile-switch-project
                projectile-load-known-projects
                projectile-find-file)
    :init
    (setq projectile-completion-system 'ivy
            projectile-switch-project-action 'projectile-find-file
            projectile-require-project-root t)
    :config
    (projectile-mode)
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
    :config
    (global-undo-tree-mode)
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

(use-package fish-mode
  :ensure t
  :commands (fish-mode)
  :delight fish-mode "Fish"
  :mode
  ("\\.fish\\'")
  )


(load-file "~/.emacs.d/keybindings.el")
(load-file "~/.emacs.d/org.el")
