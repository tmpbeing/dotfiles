;;; ../../.dotfiles/doom/.config/doom/+kubernetes.el -*- lexical-binding: t; -*-

(use-package! kubernetes
  :commands (kubernetes-overview))
;; This doesn't work for some reason so we use after! instead
;; (use-package! kubernetes-evil
;;   :after kubernetes)
(with-eval-after-load 'kubernetes
  (require 'kubernetes-evil))
