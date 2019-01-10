;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Config

;;
;; General
;;

;; UI
(setq
  doom-font (font-spec :family "Liberation Mono for Powerline" :size 14)
  doom-unicode-font (font-spec :family "DejaVu Sans" :size 14)
  doom-big-font (font-spec :family "Liberation Mono for Powerline" :size 20)
  doom-theme 'doom-tomorrow-night
  +doom-modeline-buffer-file-name-style 'truncate-upto-project
  scroll-conservatively 0
  show-trailing-whitespace t
  which-key-idle-delay 0.2)

;;
;; Keybindings
;;
(map! :ni "C-;" #'avy-goto-char-timer)

;;
;; Languages
;;

;; Rust
(setq company-racer-executable "/home/snoop/.cargo/bin/racer")

;;
;; Modules
;;

(load! "+org")
