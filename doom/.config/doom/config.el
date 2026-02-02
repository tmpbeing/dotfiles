;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Config

;;
;; General
;;

;; Use internal pin tool
(setenv "GPG_AGENT_INFO" nil)

;; UI
(setq
 company-idle-delay 0.3
 display-line-numbers-type t
 doom-font (font-spec :family "Fantasque Sans Mono" :size 18)
 doom-unicode-font (font-spec :family "DejaVu Sans" :size 16)
 doom-big-font (font-spec :family "Fantasque Sans Mono" :size 26)
 doom-theme 'doom-kanzo-zen
 doom-gruvbox-dark-variant "hard"
 doom-modeline-window-width-limit 100
 doom-modeline-buffer-encoding nil
 focus-follows-mouse t
 lsp-enable-symbol-highlighting nil
 scroll-conservatively 0
 show-trailing-whitespace t
 which-key-idle-delay 0.4)


;; Show the . and .. in dired-mode
(setq dired-omit-files "^\\.?#")

;; Projectile : ignore projects in /tmp/ and ~/.emacs.d/.local/
(setq projectile-ignored-project-function #'+projectile/ignore-project-fn)

;;
;; Keybindings
;;
(map! :ni "C-;" #'avy-goto-char-timer
      :i "C-i" #'flyspell-auto-correct-previous-word
      (:map evil-window-map ;; Adding tmux split bindings
       "\"" #'evil-window-split
       "%"  #'evil-window-vsplit))

;;
;; Evil
;;
(evil-ex-define-cmd "W" 'evil-write)
(after! evil-escape (evil-escape-mode -1)) ; Disable escape sequence
(after! evil (setq evil-ex-substitute-global t ; I like my s/../.. to by global by default
                   evil-vsplit-window-right t
                   evil-split-window-below t) ; go to the right pane on split
  )

;;
;; Ivy
;;

(setq +ivy-buffer-preview t)

;; Ivy-posframe: Decorations, put it at the top
(after! ivy-posframe
  (setf (alist-get t ivy-posframe-display-functions-alist)
        #'ivy-posframe-display-at-frame-top-center)
  (setf (alist-get 'swiper ivy-posframe-display-functions-alist)
        #'ivy-posframe-display-at-frame-top-center)
  (setq ivy-posframe-border-width 1
        ivy-posframe-width 160
        ivy-posframe-parameters (append ivy-posframe-parameters '((left-fringe . 3)
                                                                  (right-fringe . 3)))
        posframe-mouse-banish t)
  )


;;
;; Languages
;;

;; Cmake
(after! cmake-mode
  (setq cmake-tab-width 4))

;; CPP
(after! lsp-mode
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.ccls-cache$"))

;;  Elixir
(use-package! elixir-ts-extras
  :after elixir-ts-mode
  :init
  (setq! elixir-ts-extras-compilation-scroll-output t)
  ;; (transient-define-prefix elixir-ts-extras-ash-menu ()
  ;;   "Transient menu for running ash commands"
  ;;   ["Database"
  ;;    [("m ")]
  ;;    ]
  ;;   )
  (map! :localleader
        :map elixir-ts-mode-map
        "x" #'elixir-ts-extras-mix-menu
        :prefix ("t" . "test")
        "t" #'elixir-ts-extras-test-menu
        "s" #'elixir-ts-extras-test
        "f" #'elixir-ts-extras-test-file
        "a" #'elixir-ts-extras-test-all
        "r" #'elixir-ts-extras-test-rerun
        "k" #'elixir-ts-extras-test-stop))


;; Lisp
(after! lisp-mode
  (setq sly-command-switch-to-existing-lisp 'always))

;; Python

;; Rust
(setq company-racer-executable "/home/snoop/.cargo/bin/racer")

;;
;; Modules
;;

(load! "+org")
