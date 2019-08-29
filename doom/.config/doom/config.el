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
 doom-theme 'doom-base16-faded
 +modeline-buffer-path-function '+modeline-file-name
 scroll-conservatively 0
 show-trailing-whitespace t
 which-key-idle-delay 0.4)

;; Show the . and .. in dired-mode
(setq dired-omit-files "^\\.?#")

;; Projectile : ignore projects in /tmp/ and ~/.emacs.d/.local/
(setq projectile-ignored-project-function #'+projectile/ignore-project-p)

;;
;; Keybindings
;;
(map! :ni "C-;" #'avy-goto-char-timer
      (:map evil-window-map ;; Adding tmux split bindings
        "\"" #'evil-window-split
        "%"  #'evil-window-vsplit))

;;
;; Evil
;;
(evil-ex-define-cmd "W" 'evil-write)

;;
;; Languages
;;

;; Rust
(setq company-racer-executable "/home/snoop/.cargo/bin/racer")

;;
;; Modules
;;

(load! "+org")
