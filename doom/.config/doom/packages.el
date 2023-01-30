;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;; Org
(package! ob-http) ;; curl in org-mode literate programming
(package! ox-gfm)
(package! ox-jekyll :recipe (:host github :repo "tmpbeing/ox-jekyll-subtree")) ;; Org-file to jekyll blog
(package! ob-mermaid) ;; draw diagrams in org-mode with mermaid-js
(package! org-chef) ;; Import recipes from common sites into org-mode
(package! doct) ;; Better capture template declaration
(package! org-present)

;; Languages
(package! systemd)

;; Misc
(package! mermaid-mode :recipe (:host github :repo "shen390s/mermaid-mode")) ;; edit mermaid diagrams
(package! nov) ;; epub reader

;; ;; Local dev
;(package! spelunk
;  :recipe (:local-repo "spelunk" :no-byte-compile t))


;; Package configuration
(use-package! mermaid-mode
  :defer true)
(use-package! nov
  :defer true)
