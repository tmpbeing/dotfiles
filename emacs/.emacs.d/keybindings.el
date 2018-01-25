; **************************************************************************** ;
;                                                                              ;
;                                                         :::      ::::::::    ;
;    keybindings.el                                     :+:      :+:    :+:    ;
;                                                     +:+ +:+         +:+      ;
;    By: mplanell <mplanell@student.42.fr>          +#+  +:+       +#+         ;
;                                                 +#+#+#+#+#+   +#+            ;
;    Created: 2018/01/19 01:59:40 by mplanell          #+#    #+#              ;
;    Updated: 2018/01/23 06:06:35 by mplanell         ###   ########.fr        ;
;                                                                              ;
; **************************************************************************** ;

(use-package general
	:ensure t
	:config
	;(general-define-key "<escape>" 'keyboard-escape-quit)
	(general-define-key "M-x" 'counsel-M-x)
	(general-define-key "C-'" 'avy-goto-word-1)
	(general-define-key
		;:states '(normal visual insert emacs)
		:prefix "S-SPC"
		;:non-normal-prefix "C-SPC"

		;; d ;;
		"d"		'(:ignore t :which-key "describe")
		"db"	'(counsel-descbinds :which-key "binds")
		"df"	'(counsel-describe-function :which-key "function")
		"dv"	'(counsel-describe-variable :which-key "variable")

		;; f ;;
		"f"		'(:ignore t :which-key "files")
		"fa"	'(counsel-ag :which-key "ag search")
		"ff"	'(counsel-find-file :which-key "find file")
		"fg"	'(counsel-git :which-key "git ff")
		"fr"	'(counsel-recentf :which-key "recent files")
		"ft"	'(neotree-toggle :which-key "neotree")
		"fz"	'(counsel-fzf :which-key "fzf")

		;; g ;;
		"g"		'(:ignore t :which-key "go to")
		"gw"	'(avy-goto-word-1 :which-key "word")

		;; h ;;
		"h"		'(header-insert :which-key "42header")

		;; s ;;
		"s"		'swiper
		)
	)
