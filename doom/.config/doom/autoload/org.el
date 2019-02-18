;;; autoload/org.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org/open-todo-file ()
  (interactive)
  "Opens the todo file"
  (find-file +org-capture-todo-file))

(defun +org/open-bookmarks ()
  (interactive)
  "Opens the bookmark file"
  (find-file +org-capture-bookmark-file))
