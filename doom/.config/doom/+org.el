;;; +org.el -*- lexical-binding: t; -*-

(defvar +org-gong-sound
  (concat doom-private-dir "resources/gong.wav"))

(defvar +org-bell-sound
  (concat doom-private-dir "resources/prayer-bell.wav"))

(def-package! ob-http
  :after org)

(def-package! org-pomodoro
  :after org
  :config
  (setq org-pomodoro-finished-sound +org-gong-sound
        org-pomodoro-short-break-sound +org-bell-sound
        org-pomodoro-long-break-sound +org-bell-sound))

(defun +org/custom-verify-target ()
  "Exclude bookmarks (headers with a link in bookmarks.org)
and headers with DONE keywords from refile targets"
  (not (or (member (nth 2 (org-heading-components)) org-done-keywords)
           (when (equal buffer-file-name (expand-file-name "bookmarks.org" org-directory))
             (when (string-match org-bracket-link-regexp (nth 4 (org-heading-components))) t)))))

(defvar +org-capture-bookmark-file
  (expand-file-name "bookmarks.org" org-directory)
  "The path to my bookmark.

 Is relative to 'org-directory', unless it is absolute")


(after! org
  (add-to-list 'org-modules 'org-habit t)
  (setq org-bullets-bullet-list '("#")
        org-ellipsis " ▼ "
        org-refile-target-verify-function '+org/custom-verify-target)

  (setq org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* [ ] %?\n%i\n%a" :prepend t :kill-buffer t)
          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i\n%a" :prepend t :kill-buffer t)

          ;; Will use {project-root}/{todo,notes,changelog}.org, unless a
          ;; {todo,notes,changelog}.org file is found in a parent directory.
          ("p" "Templates for projects")
          ("pt" "Project todo" entry  ; {project-root}/todo.org
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)
          ("pn" "Project notes" entry  ; {project-root}/notes.org
           (file+headline +org-capture-project-notes-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)
          ("pc" "Project changelog" entry  ; {project-root}/changelog.org
           (file+headline +org-capture-project-notes-file "Unreleased")
           "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)

          ("b" "Bookmark" entry
           (file+headline +org-capture-bookmark-file "To read/Classify")
           "* [[%x][%?]] %^g" :kill-buffer t)))

  (set-popup-rule! "^\\Org Agenda"
    :size 15
    :quit t
    :select t
    :parameters
    '((transient)))

  (add-hook 'org-clock-in-hook #'save-buffer)
  (add-hook 'org-clock-out-hook #'save-buffer)

  (map! :map org-mode-map
        :localleader
        "p" #'org-pomodoro
        (:prefix ("c" . "clock")
          "r" #'org-clock-report)))

(map! :leader
      (:prefix "n"
        :desc "Org Agenda" :nvm "a" #'org-agenda-list
        :desc "Org Todo" :nvm "o" #'+org/open-todo-file
        :desc "Bookmarks" :nvm "b" #'+org/open-bookmarks)
      (:when (featurep! :completion helm)
        "X" #'helm-org-capture-templates))

;;; +org.el ends here
