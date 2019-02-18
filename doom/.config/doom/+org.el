;;; +org.el -*- lexical-binding: t; -*-

(def-package! ob-http
  :after org)

(defvar +org-capture-bookmark-file
  (expand-file-name "bookmarks.org" org-directory)
  "The path to my bookmark.

 Is relative to 'org-directory', unless it is absolute")


(after! org
  (map! :map evil-org-mode-map
        :localleader
        :desc "Create/Edit Todo" :nve "o" #'org-todo
        :desc "Schedule" :nve "s" #'org-schedule
        :desc "Deadline" :nve "d" #'org-deadline
        :desc "Refile" :nve "r" #'org-refile
        :desc "Filter" :nve "f" #'org-match-sparse-tree
        :desc "Tag heading" :nve "t" #'org-set-tags-command)
  (setq org-bullets-bullet-list '("#"))
  (setq org-ellipsis " ▼ ")
  ;; (setq org-ellipsis " ⤵")

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
         "* [%x][%?] %^g" :kill-buffer t)))

  (set-popup-rule! "^\\Org Agenda"
    :size 15
    :quit t
    :select t
    :parameters
    '((transient))))

(map! :leader
      (:prefix "o"
        :desc "Org Agenda" :nvm "a" #'org-agenda-list
        :desc "Org Todo" :nvm "o" #'+org/open-todo-file
        :desc "Bookmarks" :nvm "B" #'+org/open-bookmarks)
      (:when (featurep! :completion helm)
        "X" #'helm-org-capture-templates))

;;; +org.el ends here
