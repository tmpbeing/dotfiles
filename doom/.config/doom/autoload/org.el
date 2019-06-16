;;; autoload/org.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org/open-todo-file ()
  "Opens the todo file"
  (interactive)
  (find-file (concat org-directory +org-capture-todo-file)))

;;;###autoload
(defun +org/open-bookmarks ()
  "Opens the bookmark file"
  (interactive)
  (find-file +org-capture-bookmark-file))
