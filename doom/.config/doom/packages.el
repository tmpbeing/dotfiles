;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;; Org
(package! ob-http) ;; curl in org-mode literate programming
(package! org-pomodoro) ;; pomodoro clock directly inside org-mode
(package! ox-jekyll :recipe (:fetcher github :repo "tmpbeing/ox-jekyll-subtree")) ;; Org-file to jekyll blog

;; Misc
; (package! systemd)
