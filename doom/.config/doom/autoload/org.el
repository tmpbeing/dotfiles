;;; autoload/org.el -*- lexical-binding: t; -*-

;;;###autoload
(defun +org/open-todo-file ()
  "Opens the todo file"
  (interactive)
  (find-file (concat org-directory +org-capture-todo-file)))

;;;###autoload
(defun +org/open-blog ()
  "Opens the blog file"
  (interactive)
  (find-file (concat org-directory "blog.org")))

;;;###autoload
(defun +org/open-org-overview ()
  "Switch to org project, opens the todo file in one split and org-agenda in another."
  (interactive)
  (let ((org-agenda-window-setup 'reorganize-frame)
        (+workspaces-switch-project-function (lambda (_) (call-interactively #'+org/open-todo-file) (org-agenda nil "n"))))
    (projectile-switch-project-by-name org-directory)))


;;;###autoload
(defun +org/reload-agenda-buffer-h ()
  "Refresh all open org-agenda buffers in the current workspace"
  (dolist (buffer (+workspace-buffer-list))
    (with-current-buffer buffer (when (derived-mode-p 'org-agenda-mode)
                                  (org-agenda-redo t)))))

;;;###autoload
(defcustom +org-polybar/clock-format-style 'simple
  "Controls the content of +org/format-clock-time. Should be one of simple, long, full")

;;;###autoload
(defvar +org-polybar/last-known-state 'inactive
  "Interval variable that stores the last known state recorded on message update.")


;;; TODO: Make configurable
;;;###autoload
(defun +org-polybar/icon-from-state (state)
  "Build string for icon from the state"
  (interactive "SState :")
  (pcase state
    (':pomodoro "%{F#9b9257}\ue001")
    (':overtime "%{F#8d7856}\ue003")
    (':short-break "%{F#738c9c}\ue005")
    (':long-break "%{F#6998b3}\ue006")
    ('clock "%{F#798362}")
    (_ "%{F#acb3b5}\ue007"))
  )

;;;###autoload
(defun +org-polybar/format-status-string ()
  "Get currently clocked time string, with support for org-pomodoro and estimates, for polybar integration"
  (interactive)
                                        ; TODO: Don't require that here ? + Not interactive
  (require 'org-clock)
  (require 'org-pomodoro)
  (let* ((state (cond ((org-pomodoro-active-p) org-pomodoro-state)
                      ((org-clock-is-active) 'clock)
                      (t 'inactive)))
         (polybar-icon (+org-polybar/icon-from-state state)))
    (set '+org-polybar/last-known-state state)
    (if (or (eq +org-polybar/clock-format-style 'simple) (eq state 'inactive)) polybar-icon
      (let ((clock-timer-string (cond ((eq +org-polybar/clock-format-style 'simple) "")
                                      ((org-pomodoro-active-p) (org-pomodoro-format-seconds))
                                      ((org-clock-is-active)
                                       (let ((clocked-time (org-duration-from-minutes (org-clock-get-clocked-time))))
                                        ; Might need to org-duration-to-minutes + org-duration-from-minutes the content of org-clock-effort
                                         (if org-clock-effort (let (effort-time (org-duration-from-minutes org-clock-effort))
                                                                (format "%s/%s" clocked-time effort-time))
                                           clocked-time)))
                                      (t ""))))
        (if (or (eq +org-polybar/clock-format-style 'long)
                (member state '(:short-break :long-break)))
            (format "%s %s" polybar-icon clock-timer-string)
          (format "%s %s %s" polybar-icon clock-timer-string (org-no-properties org-clock-current-task)))))))

;;;###autoload
(defun +org-polybar/clock-format-style-toggle ()
  "Toggle between the possible states for +org-polybar/clock-format-style"
  (interactive)
  (set '+org-polybar/clock-format-style (pcase +org-polybar/clock-format-style
                                          ('simple 'long)
                                          ('long 'full)
                                          (_ 'simple))))

;;;###autoload
(defun +org-polybar/toggle-pomodoro-or-clock ()
  "Start a new pomodoro if inactive, ends current pomodoro/clock otherwise"
  (if (eq +org-polybar/last-known-state 'clock) (org-clock-out) (org-pomodoro)))

;; (defun +org-polybar/end-or-restart ()
;;   "End the currently active pomodoro or org-clock if any.
;; If inactive, will instead restart the last known pomodoro/clock as a new pomodoro.
;; ;; TODO: Restart clock as org-clock, not pomodoro ?

;; Note that it uses the last known state to determine which action to take."
;;   (pcase +org/polybar/last-known-state
;;     ('clock org-clock-out)
;;          (':pomodoro nil)
;;          (':overtime org-pomodoro-finished)
;;          (_ +org-pomodoro/restart-last-pomodoro))
;;                                         ; clock -> kill ; Can be handled separately
;;                                         ; pomodoro -> nothing
;;                                         ; overtime -> finish
;;                                         ; else -> restart
;;        )
