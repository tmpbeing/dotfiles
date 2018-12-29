;;; +org.el -*- lexical-binding: t; -*-

(def-package! ob-http
  :after org)

(setq +todo-file "~/org/todo.org")

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

  (set-popup-rule! "^\\Org Agenda"
    :size 15
    :quit t
    :select t
    :parameters
    '((transient))))

(map! :leader
      :desc "Open todo file" :nvm "O" #'+open-todo-file
      (:prefix "o"
        :desc "Org Agenda" :nvm "a" #'org-agenda-list))

;;; +org.el ends here
