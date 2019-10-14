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

;;;###autoload
(defun +org-pomodoro/restart-last-pomodoro ()
  "Starts a new pomodoro on the last clocked-in task. Resets the pomodoro count without prompt when necessary.

  This is useful for external scripts as the org-pomodoro function has y-or-n prompts"
  (require 'org-pomodoro)
  (when (and org-pomodoro-last-clock-in
             org-pomodoro-expiry-time
             (org-pomodoro-expires-p))
    (setq org-pomodoro-count 0))
  (setq org-pomodoro-last-clock-in (current-time))

  (call-interactively 'org-clock-in-last)
  (org-pomodoro-start :pomodoro))

;;;###autoload
(defun +org-pomodoro/start-pomodoro-on-capture ()
  "Starts org-pomodoro upon capture if the pomodoro capture template was used"

  (require 'org-pomodoro)
  (when (and (not org-note-abort)
             (equal (org-capture-get :pomodoro) t))
    (when (and org-pomodoro-last-clock-in
               org-pomodoro-expiry-time
               (org-pomodoro-expires-p))
      (setq org-pomodoro-count 0))
    (set-buffer (org-capture-get :buffer))
      (goto-char (org-capture-get :insertion-point))
      (org-clock-in)
      (org-pomodoro-start :pomodoro)))
