;;; autoload/python.el -*- lexical-binding: t; -*-

;;;###autoload
;;; Not named +python cause it seems treesit doesn't like that
(defun python-treesit-filter-self (node)
  (not (equal (treesit-node-text node) "self")))

;;;###autoload
(defun +python/apply-treesit-custom-rules ()
  ;; Only applies on font lock level 4
  (unless (member 'custom (nth 3 treesit-font-lock-feature-list))
    (push 'custom (nth 3 treesit-font-lock-feature-list)))
  (setq-local treesit-font-lock-settings
              (append python--treesit-settings
                      (treesit-font-lock-rules

                       :language 'python
                       :override t
                       :feature 'custom
                       '((decorator "@" @font-lock-constant-face))

                       :language 'python
                       :override t
                       :feature 'custom
                       '((keyword_argument name: (identifier) @font-lock-keyword-face))

                       :language 'python
                       :override t
                       :feature 'custom
                       '(((parameters (identifier) @font-lock-keyword-face
                           (:pred python-treesit-filter-self @font-lock-keyword-face)))

                         (parameters (typed_parameter (identifier) @font-lock-keyword-face))
                         (parameters (default_parameter name: (identifier) @font-lock-keyword-face))
                         (parameters (typed_default_parameter name: (identifier) @font-lock-keyword-face))

                         (parameters
                          (list_splat_pattern ; *args
                           (identifier) @font-lock-keyword-face))
                         (parameters
                          (dictionary_splat_pattern ; **kwargs
                           (identifier) @font-lock-keyword-face))

                         (lambda_parameters
                          (identifier) @font-lock-keyword-face))

                       :language 'python
                       :override t
                       :feature 'custom
                       '((keyword_argument name: (identifier) @font-lock-keyword-face)))))
  (treesit-font-lock-recompute-features)
  (font-lock-flush)
  (font-lock-ensure))
