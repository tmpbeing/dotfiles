;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;; Org
(package! ob-http) ;; curl in org-mode literate programming
(package! ox-jekyll :recipe (:host github :repo "tmpbeing/ox-jekyll-subtree")) ;; Org-file to jekyll blog
(package! org-trello)

;; Python
(package! sphinx-doc)

;; Themes
(package! vibrant-ink-theme :recipe (:host github :repo "arkhan/vibrant-ink-theme" :files ("vibrant-ink-theme.el")))

;; Misc
; (package! systemd)
