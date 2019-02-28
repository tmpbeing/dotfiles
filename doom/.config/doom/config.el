;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Config

;;
;; General
;;

;; UI
(setq
  doom-font (font-spec :family "SF Mono" :size 14)
  doom-unicode-font (font-spec :family "DejaVu Sans" :size 14)
  doom-big-font (font-spec :family "SF Mono" :size 18)
  doom-theme 'doom-ayu-dark
  +modeline-buffer-path-function '+modeline-file-name
  scroll-conservatively 0
  show-trailing-whitespace t
  which-key-idle-delay 0.4)

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
