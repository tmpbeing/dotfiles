;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el


;; Languages
(package! systemd)
(package! kdl-mode)
(package! mermaid-mode :recipe (:host github :repo "shen390s/mermaid-mode")) ;; edit mermaid diagrams
(package! elixir-ts-extras
  :recipe (:host github :repo "wkirschbaum/elixir-ts-extras")
  )
;; elixir-ts-extras provides better testing functions for elixir
(package! exunit :disable t)

;; Org
(package! ob-http) ;; curl in org-mode literate programming
(package! ox-gfm)
(package! ox-jekyll :recipe (:host github :repo "tmpbeing/ox-jekyll-subtree")) ;; Org-file to jekyll blog
(package! org-chef) ;; Import recipes from common sites into org-mode
(package! doct) ;; Better capture template declaration
(package! ob-mermaid) ;; draw diagrams in org-mode with mermaid-js
(package! org-media-note :recipe (:host github :repo "yuchen-lea/org-media-note"))

;; Misc
(package! nov) ;; epub reader
(package! night-owl-theme)
(package! doom-moonfly-theme
  :recipe (:host github
           :repo "stackmystack/doom-moonfly-theme"))
(package! kubernetes)
(package! kubernetes-evil)
(package! jj-mode :recipe (:host github :repo "bolivier/jj-mode.el"))
(package! mason)
(package! difftastic)
(package! magit-todos)


;; Local dev

;; (package! spelunk
;; recipe (:local-repo "spelunk" :no-byte-compile t))
