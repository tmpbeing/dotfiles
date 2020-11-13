;;; +org.el -*- lexical-binding: t; -*-

(defvar +org-gong-sound
  (concat doom-private-dir "resources/gong.wav")
  "Sound used by org-pomodoro")

(defvar +org-bell-sound
  (concat doom-private-dir "resources/prayer-bell.wav")
  "Sound used by org-pomodoro")

(use-package! ob-http
  :after org)

(use-package! ox-gfm
  :after org)

(after! org-pomodoro
  (setq org-pomodoro-manual-break t
        org-pomodoro-finished-sound +org-gong-sound
        org-pomodoro-overtime-sound +org-gong-sound
        org-pomodoro-short-break-sound +org-bell-sound
        org-pomodoro-long-break-sound +org-bell-sound))

(use-package! ox-jekyll
  :after org
  :config
  (setq org-jekyll-use-src-plugin t
        ojs-blog-base-url "http://planelles.dev"
        ojs-blog-dir (expand-file-name "~/code/blog")))

(use-package! ob-mermaid
  :after org
  :init
  (setq ob-mermaid-cli-path "/usr/bin/mmdc")
  )

(setq org-directory "~/Dropbox/org/"
      org-default-notes-file (concat org-directory "notes.org")
      org-bullets-bullet-list '("#")
      org-superstar-headline-bullets-list '("#")
      org-ellipsis " ▼ "
      org-agenda-files (list (concat org-directory +org-capture-todo-file))
      org-agenda-inhibit-startup nil
      org-agenda-include-deadlines t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-archive-location (concat org-directory "archive.org::")
      org-log-done 'time
      org-startup-folded 'fold
      org-use-property-inheritance t
      org-list-allow-alphabetical t ; allows a) A) bullets
      +org-capture-journal-file (concat org-directory "journal.org"))

(set-popup-rule! "^\\Org Agenda"
  :size 15
  :quit t
  :select t
  :parameters
  '((transient)))

(add-hook! org-mode :append
  (add-hook! after-save :append :local #'+org/reload-agenda-buffer-h))
(add-hook! '(org-clock-in-hook org-clock-out-hook) #'save-buffer)

;;; +org.el ends here
