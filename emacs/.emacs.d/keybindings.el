; **************************************************************************** ;
;                                                                              ;
;                                                         :::      ::::::::    ;
;    keybindings.el                                     :+:      :+:    :+:    ;
;                                                     +:+ +:+         +:+      ;
;    By: mplanell <mplanell@student.42.fr>          +#+  +:+       +#+         ;
;                                                 +#+#+#+#+#+   +#+            ;
;    Created: 2018/01/19 01:59:40 by mplanell          #+#    #+#              ;
;    Updated: 2018/01/28 03:48:08 by mplanell         ###   ########.fr        ;
;                                                                              ;
; **************************************************************************** ;

(use-package general
	:ensure t
	:config
	;(general-define-key "<escape>" 'keyboard-escape-quit)
	
	;; Global binds ;;
	(general-define-key "M-x" 'counsel-M-x)
	(general-define-key
		:states '(normal visual insert emacs)
		:prefix "SPC"
		:non-normal-prefix "C-SPC"

		;; c ;;
		"c"			'(:ignore t :which-key "comment")
		"cb"		'(comment-box :which-key "box")
		"cc"		'(evilnc-comment-or-uncomment-lines :which-key "line")
		"cl"		'(comments-insert-bar :which-key "bar")
		"cp"		'(evilnc-comment-or-uncomment-paragraphs :which-key "paragraph")

		;; d ;;
		"d"			'(:ignore t :which-key "describe")
		"db"		'(counsel-descbinds :which-key "binds")
		"df"		'(counsel-describe-function :which-key "function")
		"dv"		'(counsel-describe-variable :which-key "variable")

		;; f ;;
		"f"			'(:ignore t :which-key "files")
		"fa"		'(counsel-ag :which-key "ag search")
		"ff"		'(counsel-find-file :which-key "find file")
		"fg"		'(counsel-git :which-key "git ff")
		"fp"		'(counsel-git-grep :which-key "git grep")
		"fr"		'(counsel-recentf :which-key "recent files")
		"ft"		'(neotree-toggle :which-key "neotree")
		"fz"		'(counsel-fzf :which-key "fzf")

		;; g ;;
		"g"			'(:ignore t :which-key "go to")
		"gt"		'(avy-goto-word-1 :which-key "word")
		"gl"		'(avy-goto-line :which-key "line")
		"go"		'(avy-org-goto-heading-timer "org heading")

		;; h ;;
		"h"			'(header-insert :which-key "42header")

		;; o ;;
		"o"			'(:ignore t :which-key "org")
		"oa"		'(org-agenda :which-key "agenda")
		"oc"		'(org-capture :which-key "capture")

		;; s ;;
		"s"		'swiper
		)

	;; insert state binds ;;
	(general-define-key
		:states '(insert))
		"C-x C-f"	'company-files

	;; Mode-specific keybindings ;;

	;; Dired keybindings ;;
	(general-define-key
		:states '(emacs normal)
		:keymaps 'dired-mode-map
		"RET"		'dired-find-file
		"TAB"		'dired-subtree-toggle
		"<backtab>" 'dired-subtree-cycle
		"/"			'swiper
		"B"			'dired-do-bytecompile
		"C"			'dired-do-copy
		"d"			'dired-flag-file-deletion
		"D"			'dired-do-delete
		"F"			'find-name-dired
		"gg"		'evil-goto-first-line
		"G"			'evil-goto-line
		"h"			'left-char
		"j"			'dired-next-line
		"k"			'dired-previous-line
		"l"			'right-char
		"L"			'dired-do-load
		"m"			'dired-mark
		"O"			'dired-omit-mode
		"q"			'quit-window
		"R"			'dired-do-rename
		"t"			'dired-toggle-marks
		"u"			'dired-unmark
		"U"			'dired-unmark-all-files
		"x"			'dired-do-flagged-delete
		"+"			'dired-create-directory
		)

	;; Ivy (occur) keybindings ;;
	(general-define-key
		:states '(normal)
		:keymaps 'ivy-occur-mode-map
		"RET"		'ivy-occur-press-and-switch
		"g"			nil
		"ga"		'ivy-occur-read-action
		"gc"		'ivy-occur-toggle-calling
		"gg"		'evil-goto-first-line
		"gf"		'ivy-occur-press
		"h"			'evil-backward-char
		"j"			'ivy-occur-next-line
		"k"			'ivy-occur-previous-line
		"l"			'evil-forward-char
		"q"			'quit-window
	  )
	)
