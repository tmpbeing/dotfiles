;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Config

;;
;; General
;;

;; Use internal pin tool
(setenv "GPG_AGENT_INFO" nil)

;; UI
(setq
 doom-font (font-spec :family "SF Mono" :size 14)
 doom-unicode-font (font-spec :family "DejaVu Sans" :size 14)
 doom-big-font (font-spec :family "SF Mono" :size 20)
 doom-theme 'doom-base16-faded
 doom-gruvbox-dark-variant "hard"
 doom-modeline-window-width-limit 100
 doom-modeline-buffer-encoding nil
 scroll-conservatively 0
 show-trailing-whitespace t
 which-key-idle-delay 0.4)

(setq lsp-enable-symbol-highlighting nil)

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

;; Cmake
(after! cmake-mode
  (setq cmake-tab-width 4))

;; CPP
(after! lsp-mode
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.ccls-cache$"))

;; Python
(after! lsp-python-ms
  (set-lsp-priority! 'mspyls 1))
(setq lsp-python-ms-extra-paths (list "/home/snoop/code/ExtralityDockerAPI/services"))
;; (setq-hook! 'python-mode-hook flycheck-python-mypy-executable "/usr/bin/mypy")
;; (setq-hook! 'python-mode-hook flycheck-python-flake8-executable "/usr/bin/flake8")

;; Rust
(setq company-racer-executable "/home/snoop/.cargo/bin/racer")

;;
;; Modules
;;

(load! "+org")
