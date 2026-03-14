;;; +org.el -*- lexical-binding: t; -*-

(defvar +org-gong-sound
  (concat doom-user-dir "resources/gong.wav")
  "Sound used by org-pomodoro")

(defvar +org-bell-sound
  (concat doom-user-dir "resources/prayer-bell.wav")
  "Sound used by org-pomodoro")

(defvar +org-capture-recipes (concat org-directory "recipes.org")
  "Org file in which to store org-chef/capture recipes")

(use-package! ob-http
  :after org)

(use-package! ox-gfm
  :after org)

(use-package! ox-jekyll
  :after org
  :config
  (setopt org-jekyll-use-src-plugin t
          ojs-blog-base-url "http://planelles.dev"
          ojs-blog-dir (expand-file-name "~/code/blog")))

(use-package! ob-mermaid
  :after org
  :init
  (setopt ob-mermaid-cli-path "/usr/bin/mmdc")
  )

(use-package! doct
  :commands doct
  :config
  ;; Add :icons to doct, full credit to tecosaur https://tecosaur.github.io/emacs-config/config.html#capture
  (with-eval-after-load 'org-capture
    (defun +doct-icon-declaration-to-icon (declaration)
      "Convert :icon declaration to icon"
      (let ((name (pop declaration))
            (set  (intern (concat "nerd-icons-" (plist-get declaration :set))))
            (face (intern (concat "nerd-icons-" (plist-get declaration :color))))
            (v-adjust (or (plist-get declaration :v-adjust) 0.01)))
        (apply set `(,name :face ,face :v-adjust ,v-adjust))))

    (defun +doct-iconify-capture-templates (groups)
      "Add declaration's :icon to each template group in GROUPS."
      (let ((templates (doct-flatten-lists-in groups)))
        (setopt doct-templates (mapcar (lambda (template)
                                         (when-let* ((props (nthcdr (if (= (length template) 4) 2 5) template))
                                                     (spec (plist-get (plist-get props :doct) :icon)))
                                           (setf (nth 1 template) (concat (+doct-icon-declaration-to-icon spec)
                                                                          "\t"
                                                                          (nth 1 template))))
                                         template)
                                       templates))))

    (setopt doct-after-conversion-functions '(+doct-iconify-capture-templates))
    ))

(use-package! org-chef
  :commands (org-chef-insert-recipe org-chef-get-recipe-from-url))

(use-package! org-media-note
  :hook (org-mode . org-media-note-mode)
  :init
  (map! :after org
        :map org-mode-map
        :localleader
        "M" #'org-media-note-show-interface
        )
  :config
  (setopt org-media-note-screenshot-image-dir (concat org-directory "screenshots/"))
  )

(with-eval-after-load 'org-capture
  (setopt org-capture-templates
          (doct '(("Todo"
                   :keys "t"
                   :icon ("nf-oct-checklist" :set "octicon" :color "green")
                   :file +org-capture-todo-file
                   :prepend t
                   :headline "Inbox"
                   :type entry
                   :template ("* TODO %?"
                              "%i %a")
                   )
                  ("Recipe"
                   :keys "r"
                   :icon ("nf-fa-spoon" :set "faicon" :color "dorange")
                   :file +org-capture-recipes
                   :headline "Unsorted"
                   :template "%(org-chef-get-recipe-from-url)")
                  ;; ("Meeting"
                  ;;  :keys "m"
                  ;;  :icon ()
                  ;;  :type entry
                  ;;  :file org-roam-dailies-goto-today
                  ;;  :template ("* MEETING %?"
                  ;;             "%i")
                  ;;  )
                  ))))

(setopt org-directory "~/Dropbox/org/"
        org-default-notes-file (concat org-directory "notes.org")
        org-bullets-bullet-list '("#")
        org-superstar-headline-bullets-list '("#")
        org-ellipsis " ▼ "
        org-agenda-files (list (concat org-directory +org-capture-todo-file))
        org-agenda-inhibit-startup nil
        org-agenda-include-deadlines t
        org-agenda-skip-deadline-if-done t
        org-agenda-skip-scheduled-if-done t
        org-archive-location (concat org-directory "archive.org::datetree/")
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

(with-eval-after-load 'org-pomodoro
  (setopt org-pomodoro-manual-break t
          org-pomodoro-finished-sound +org-gong-sound
          org-pomodoro-overtime-sound +org-gong-sound
          org-pomodoro-short-break-sound +org-bell-sound
          org-pomodoro-long-break-sound +org-bell-sound))

;;; +org.el ends here
