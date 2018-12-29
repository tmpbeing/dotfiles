;;; autoload/org.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org/open-todo-file ()
  (interactive)
  "Opens the todo file"
  (find-file +todo-file))
