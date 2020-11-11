;;; autoload/org.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org/open-todo-file ()
  "Opens the todo file"
  (interactive)
  (find-file (concat org-directory +org-capture-todo-file)))

;;;###autoload
(defun +org/open-blog ()
  "Opens the blog file"
  (interactive)
  (find-file (concat org-directory "blog.org")))

;;;###autoload
(defun +org/open-org-overview ()
  "Switch to org project, opens the todo file in one split and org-agenda in another."
  (interactive)
  (projectile-switch-project-by-name org-directory)
  (+org/open-todo-file)
  (split-window-right)
  (let ((org-agenda-window-setup 'other-window))
    (org-agenda nil "a")))
