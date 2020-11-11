;;; autoload/projectile.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +projectile/ignore-project-fn (root)
  "Ignores project that are in tmp or part of .emacs.d/.local"
  (or (string-prefix-p "/tmp/" root)
      (string-match-p (regexp-quote "/.emacs.d/.local/") root)))
