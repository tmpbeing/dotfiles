; **************************************************************************** ;
;                                                                              ;
;                                                         :::      ::::::::    ;
;    keybindings.el                                     :+:      :+:    :+:    ;
;                                                     +:+ +:+         +:+      ;
;    By: mplanell <mplanell@student.42.fr>          +#+  +:+       +#+         ;
;                                                 +#+#+#+#+#+   +#+            ;
;    Created: 2018/01/19 01:59:40 by mplanell          #+#    #+#              ;
;    Updated: 2018/01/29 03:42:00 by mplanell         ###   ########.fr        ;
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

		;; b ;;
		"b"			'(hydra-buffer/body :which-key "buffer")

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

		;; p ;;
		"p"			'(hydra-projectile/body :which-key "projectile")

		;; s ;;
		"s"		'swiper

		;; w ;;
		"w"			'(hydra-window/body :which-key "windows")
		)

	;; insert state binds ;;
	(general-define-key
		:states '(insert))
		"C-x C-f"	'company-files

	;; motion states binds
	(general-define-key
		:states '(motion))
		"/"			'swiper

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

	;; Ibuffer ;;
	(general-define-key
		:states '(emacs)
		:keymaps 'ibuffer-mode-map
		"/"			'swiper
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

	;; Undo-tree ;;
	(general-define-key
		:states '(motion)
		:keymaps 'undo-tree-visualizer-mode-map
		"h"			'undo-tree-visualize-left
		"j"			'undo-tree-visualize-redo
		"k"			'undo-tree-visualize-undo
		"l"			'undo-tree-visualize-right
		)

	;; Hydras ;; NEED TO REDO THESE
	;; Buffer ;;
	(defhydra hydra-buffer (:color blue :columns 3)
		"
		              Buffers :
		"
		("n" next-buffer "next" :color red)
		("b" ivy-switch-buffer "switch")
		("B" ibuffer "ibuffer")
		("p" previous-buffer "prev" :color red)
		("C-b" buffer-menu "buffer menu")
		("N" evil-buffer-new "new")
		("d" kill-this-buffer "delete" :color red)
		("D" (progn (kill-this-buffer) (next-buffer)) "Delete" :color red)
		("s" save-buffer "save" :color red))

	;; Projectile ;;
	(defhydra hydra-projectile
		(:color teal :hint nil)
		"
		   PROJECTILE: %(projectile-project-root)
		
		^Find File^        ^Search/Tags^        ^Buffers^       ^Cache^                    ^Project^
		^---------^        ^-----------^        ^-------^       ^-----^                    ^-------^
		_f_: file          _a_: ag              _i_: Ibuffer    _c_: cache clear           _p_: switch proj
		_F_: file dwim     _g_: update gtags    _b_: switch to  _x_: remove known project
		_C-f_: file pwd    _o_: multi-occur   _s-k_: Kill all   _X_: cleanup non-existing
		_r_: recent file   ^ ^                  ^ ^             _z_: cache current
		_d_: dir
"
		("a"   projectile-ag)
		("b"   projectile-switch-to-buffer)
		("c"   projectile-invalidate-cache)
		("d"   projectile-find-dir)
		("f"   projectile-find-file)
		("F"   projectile-find-file-dwim)
		("C-f" projectile-find-file-in-directory)
		("g"   ggtags-update-tags)
		("s-g" ggtags-update-tags)
		("i"   projectile-ibuffer)
		("K"   projectile-kill-buffers)
		("s-k" projectile-kill-buffers)
		("m"   projectile-multi-occur)
		("o"   projectile-multi-occur)
		("p"   projectile-switch-project)
		("r"   projectile-recentf)
		("x"   projectile-remove-known-project)
		("X"   projectile-cleanup-known-projects)
		("z"   projectile-cache-current-file)
		("q"   nil "cancel" :color blue))

	;; Window ;;
	(defhydra hydra-window (:hint nil)
		  "
		        Split: _v_ert  _s_:horz
		       Delete: _c_lose  _o_nly
		Switch Window: _h_:left  _j_:down  _k_:up  _l_:right
		      Buffers: _p_revious  _n_ext  _b_:select  _f_ind-file  _F_projectile
		       Winner: _u_ndo  _r_edo
		       Resize: _H_:splitter left  _J_:splitter down  _K_:splitter up  _L_:splitter right
		         Move: _a_:up  _z_:down  _i_menu"
		
		
			("z" scroll-up-line)
			("a" scroll-down-line)
			("i" idomenu)
			
			("u" winner-undo)
			("r" winner-redo)
			
			("h" windmove-left)
			("j" windmove-down)
			("k" windmove-up)
			("l" windmove-right)
			
			("p" previous-buffer)
			("n" next-buffer)
			("b" ido-switch-buffer) 
			("f" ido-find-file)
			("F" projectile-find-file)
			
			("s" split-window-below)
			("v" split-window-right)
			
			("c" delete-window)
			("o" delete-other-windows)
			
			("H" hydra-move-splitter-left)
			("J" hydra-move-splitter-down)
			("K" hydra-move-splitter-up)
			("L" hydra-move-splitter-right)
			
			("q" nil))
	)
