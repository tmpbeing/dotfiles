;;; init.el -*- lexical-binding: t; -*-

(doom! :input
       ;;chinese
       ;;japanese
       ;;layout

       :checkers
       ;;grammar
       (syntax
        +childframe)
       (spell
        +flyspell
        +aspell)

       :completion
       (company            ; the ultimate code completion backend
        +auto
        +childframe)
       ;;helm              ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...
       ;; (ivy                ; a search engine for love and life
       ;;  +childframe
       ;;  +icons
       ;;  +prescient)
       vertico

       :ui
       ;;deft              ; notational velocity for Emacs
       doom                ; what makes DOOM look the way it does
       doom-dashboard      ; a nifty splash screen for Emacs
       doom-quit           ; DOOM quit-message prompts when you quit Emacs
       ;;emoji
       fill-column         ; a `fill-column' indicator
       hl-todo             ; highlight TODO/FIXME/NOTE tags
       ;;hydra
       indent-guides       ; highlighted indent columns
       ;;ligatures
       ;;minimap
       modeline            ; snazzy, Atom-inspired modeline, plus API
       nav-flash           ; blink the current line after jumping
       ;;neotree           ; a project drawer, like NERDTree for vim
       ophints             ; highlight the region an operation acts on
       (popup              ; tame sudden yet inevitable temporary windows
        +all               ; catch all popups that start with an asterix
        +defaults)         ; default popup rules
       ;;tabs              ; FIXME an (incomplete) tab bar for Emacs
       treemacs            ; a project drawer, like neotree but cooler
       ;;unicode           ; extended unicode support for various languages
       vc-gutter           ; vcs diff in the fringe
       vi-tilde-fringe     ; fringe tildes to mark beyond EOB
       window-select       ; visually switch windows
       workspaces          ; tab emulation, persistence & separate workspaces
       zen

       :editor
       (evil +everywhere)  ; come to the dark side, we have cookies
       file-templates      ; auto-snippets for empty files
       fold                ; (nigh) universal code folding
       (format             ; automated prettiness
        +onsave)
       ;;god
       ;;lispy             ; vim for lisp, for people who dont like vim
       multiple-cursors    ; editing in many places at once
       ;;objed
       ;;parinfer          ; turn lisp into python, sort of
       rotate-text         ; cycle region at point between text candidates
       snippets            ; my elves. They type so I don't have to
       ;;word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired              ; making dired pretty [functional]
        ;;+ranger          ; bringing the goodness of ranger to dired
        +icons)            ; colorful icons for dired-mode
       electric            ; smarter, keyword-based electric-indent
       (ibuffer             ; interactive buffer management
        +icons)
       undo
       ;; +tree)
       vc                  ; version-control and Emacs, sitting in a tree

       :term
       eshell              ; a consistent, cross-platform shell (WIP)
       ;;shell             ; a terminal REPL for Emacs
       ;;term              ; terminals in Emacs
       vterm               ; another terminals in Emacs

       :tools
       ;;ansible
       ;;biblio
       (debugger +lsp)     ; FIXME stepping through code, to help you add bugs
       direnv
       (docker +lsp)
       editorconfig        ; let someone else argue about tabs vs spaces
       ein                 ; tame Jupyter notebooks with emacs
       (eval               ; run code, run (also, repls)
        +overlay)
       gist                ; interacting with github gists
       (lookup             ; helps you navigate your code and documentation
        ;;+dictionary
        ;;+offline
        +docsets)          ; ...or in Dash docsets locally
       lsp
       (magit              ; a git porcelain for Emacs
        +forge)
       make                ; run make tasks from Emacs
       ;;(pass              ; password manager for nerds
       ;;+auth)
       pdf                 ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       rgb                 ; creating color strings
       taskrunner
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       tree-sitter
       upload              ; map local to remote projects via ssh/ftp

       :lang
       ;;agda              ; types of types of types of types...
       ;;beancount
       (cc                 ; C/C++/Obj-C madness
        +lsp)
       ;;clojure           ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       ;;(dart +flutter)
       data                ; config/data formats
       (elixir             ; erlang done right
        +lsp
        +tree-sitter)
       ;;elm               ; care for a cup of TEA?
       erlang              ; an elegant language for a more civilized age
       emacs-lisp          ; drown in parentheses
       ;;ess               ; emacs speaks statistics
       ;;factor
       ;;faust
       ;;fsharp
       ;;fstar
       ;;gdscript
       (go                 ; the hipster dialect
        +lsp
        +tree-sitter)
       (haskell +intero)   ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ;
       ;;(java             ; the poster child for carpal tunnel syndrome
       ;;+meghanada
       ;;+lsp)
       (javascript         ; all(hope(abandon(ye(who(enter(here))))))
        +lsp
        +tree-sitter)
       (json
        +lsp)
       ;;julia             ; a better, faster MATLAB
       ;(kotlin +lsp)      ; a better, slicker Java(Script)
       (latex)             ; writing papers in Emacs has never been so fun
       ;;+latexmk
       ;;+cdlatex
       ;;+fold
       ;;+lsp)
       ;;lean
       ;;ledger            ; an accounting system in Emacs
       ;;lua               ; one-based indices? one-based indices
       (markdown           ; writing docs for people to ignore
        +grip)
       ;;nim               ; python + lisp at the speed of c
       nix                 ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       (org                ; organize your plain life in plain text
        ;;+brain
        +dragndrop
        ;;+gnuplot
        ;;+hugo
        ;;+journal
        ;;+jupyter
        ;;+noter
        +pandoc
        +pomodoro
        ;;+pretty
        +present)
        ;;+roam)
       ;;perl              ; write code no one else can comprehend
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       (python             ; beautiful is better than ugly
        +lsp
        +pyright
        +tree-sitter)
       ;;+pyenv
       ;;+cython
       ;;+conda
        +poetry)
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       ;;raku
       rest                ; Emacs as a REST client
       rst                 ; ReST in peace
       ;;(ruby             ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;;+lsp
       ;;+rvm
       ;;+rbenv
       ;;+chruby
       ;;+rails
       (rust               ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
        +lsp
        +tree-sitter)
       ;;scheme
       (sh                 ; she sells (ba|z|fi)sh shells on the C xor
        +fish
        +lsp
        +tree-sitter)
       ;;sml
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       (web                ; the tubes
        +lsp
        +tree-sitter)
       (yaml
        +lsp)
       ;;zig

       :email
       ;;(mu4e +gmail)
       ;;(notmuch +afew)
       ;;(wanderlust +gmail)

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :app
       ;;calendar
       ;;irc               ; how neckbeards socialize
       (rss +org)          ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought
       ;; +langtool)       ; a proofreader (grammar/style check) for Emacs

       :config
       ;; For literate config users. This will tangle+compile a config.org
       ;; literate config in your `doom-private-dir' whenever it changes.
       ;;literate

       ;; The default module sets reasonable defaults for Emacs. It also
       ;; provides a Spacemacs-inspired keybinding scheme and a smartparens
       ;; config. Use it as a reference for your own modules.
       (default +bindings +smartparens))
