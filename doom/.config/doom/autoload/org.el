;;; autoload/org.el -*- lexical-binding: t; -*-

;;;###autoload
(defvar +org-gong-sound
  (concat doom-private-dir "resources/gong.wav")
  "Sound used by org-pomodoro")

;;;###autoload
(defvar +org-bell-sound
  (concat doom-private-dir "resources/prayer-bell.wav")
  "Sound used by org-pomodoro")

;;;###autoload
(defvar +org-capture-bookmark-file
  (expand-file-name "bookmarks.org" org-directory)
  "The path to my bookmark.

 Is relative to 'org-directory', unless it is absolute")


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

;;;###autoload
(defun +org/open-blog ()
  "Opens the blog file"
  (interactive)
  (find-file (concat org-directory "blog.org")))

;;;###autoload
(defun +org/custom-verify-target ()
  "Exclude bookmarks (headers with a link in bookmarks.org)
and headers with DONE keywords from refile targets"
  (not (or (member (nth 2 (org-heading-components)) org-done-keywords)
           (when (equal buffer-file-name (expand-file-name "bookmarks.org" org-directory))
             (when (string-match org-bracket-link-regexp (nth 4 (org-heading-components))) t)))))
