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
 doom-font (font-spec :family "TX02 Nerd Font" :size 16)
 doom-symbol-font (font-spec :family "TX02 Nerd Font" :size 16)
 doom-big-font (font-spec :family "TX02 Nerd Font" :size 22)
 doom-theme 'doom-kanzo-zen
 doom-gruvbox-dark-variant "hard"
 doom-catppuccin-dark-variant "mocha"
 doom-gruvbox-material-dark-variant "hard"
 doom-modeline-window-width-limit 100
 doom-modeline-buffer-encoding nil
 focus-follows-mouse t
 lsp-enable-symbol-highlighting t
 scroll-conservatively 0
 show-trailing-whitespace t
 which-key-idle-delay 0.4)


;; Show the . and .. in dired-mode
(setq dired-omit-files "^\\.?#")

;; Projectile : ignore projects in /tmp/ and ~/.emacs.d/.local/
(setq projectile-ignored-project-function #'+projectile/ignore-project-fn
      projectile-project-search-path "~/code")

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
(with-eval-after-load 'evil-escape (evil-escape-mode -1)) ; Disable escape sequence
(with-eval-after-load 'evil (setq evil-ex-substitute-global t ; I like my s/../.. to be global by default
                                  evil-vsplit-window-right t
                                  evil-split-window-below t) ; go to the right pane on split
                      )

;;
;; LSP
;;
(with-eval-after-load 'lsp-ui
  (setq lsp-ui-doc-enable nil
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-symbol nil
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-show-hover nil
        lsp-ui-sideline-show-diagnostics t))

;;
;; Magit and co
;;
(with-eval-after-load 'code-review
  (setq code-review-auth-login-marker 'forge))


;;
;; Languages
;;

;; Cmake
(with-eval-after-load 'cmake-mode
  (setq cmake-tab-width 4))

;; CPP
(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.ccls-cache$"))

;;  Elixir
(use-package! elixir-ts-extras
  :after elixir-ts-mode
  :init
  (setopt elixir-ts-extras-compilation-scroll-output t)
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

;; https://github.com/doomemacs/doomemacs/issues/7537
(add-to-list '+whitespace-guess-excluded-modes 'elixir)
(add-to-list '+whitespace-guess-excluded-modes 'elixir-ts-mode)

;; Lisp
(with-eval-after-load 'lisp-mode
  (setq sly-command-switch-to-existing-lisp 'always))

;; Ocaml
;; (add-to-list 'load-path "/home/snoop/.opam/default/share/emacs/site-lisp")
;; (require 'ocp-indent)

;; Python
(set-formatter! 'project-ruff '("apheleia-from-project-root" "pyproject.toml"
                                "ruff" "format" "--silent"
                                (apheleia-formatters-fill-column "--line-length")
                                "--stdin-filename" filepath "-")
  :modes '(python-mode python-ts-mode))

(with-eval-after-load 'python
  (add-hook 'python-ts-mode-hook #'+python/apply-treesit-custom-rules))


;; Rust
(setq company-racer-executable "/home/snoop/.cargo/bin/racer")

;; Misc
(with-eval-after-load 'gptel
  (setopt
   gptel-backend (gptel-make-gh-copilot "Copilot")))

(use-package! mason
  :config
  (mason-setup))

(use-package! difftastic
  :after magit
  :config
  '(transient-append-suffix 'magit-diff '(-1 -1)
     [("D" "Difftastic diff (dwim" difftastic-magit-diff)
      ("S" "Difftastic show" difftastic-magit-show)]))

(use-package! magit-todos
  :after magit
  :config (magit-todos-mode 1))

(use-package! atomic-chrome
  :defer 3
  :when (display-graphic-p)
  :commands atomic-chrome-start-server)

;; Modules
;;

(load! "+org")
(load! "+kubernetes")
