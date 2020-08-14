;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;; Org
(package! ob-http) ;; curl in org-mode literate programming
(package! ox-jekyll :recipe (:host github :repo "tmpbeing/ox-jekyll-subtree")) ;; Org-file to jekyll blog
(package! ob-mermaid) ;; draw diagrams in org-mode with mermaid-js

;; Misc
(package! mermaid-mode :recipe (:host github :repo "shen390s/mermaid-mode")) ;; edit mermaid diagrams
(package! nov) ;; epub reader
