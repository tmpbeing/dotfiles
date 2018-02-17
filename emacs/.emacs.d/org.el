; **************************************************************************** ;
;                                                                              ;
;                                                         :::      ::::::::    ;
;    org.el                                             :+:      :+:    :+:    ;
;                                                     +:+ +:+         +:+      ;
;    By: mplanell <mplanell@student.42.fr>          +#+  +:+       +#+         ;
;                                                 +#+#+#+#+#+   +#+            ;
;    Created: 2018/01/19 01:59:47 by mplanell          #+#    #+#              ;
;    Updated: 2018/02/08 20:45:14 by mplanell         ###   ########.fr        ;
;                                                                              ;
; **************************************************************************** ;

(use-package org
    :commands (org-mode
                org-agenda-list
                org-capture
                org-store-link
                org-agenda)
    :mode (("\\.org\\'" . org-mode)
        ("*Org Agenda*" . org-agenda-mode))
    :config
    (doom-themes-org-config)
    (setq org-directory "/home/snoop/org"
        org-default-notes-file (concat org-directory "/notes.org")
        org-agenda-start-on-weekday 1
        org-todo-keywords '((sequence "☞ TODO(t)" "☛ NEXT(n)" "⚑ WAITING(w@)" "|" "✔ DONE(d!)" "✘ CANCELED(c@)"))
        org-src-fontify-natively t)
    )

;;; Prettier bullets
(use-package org-bullets
    :ensure t
    :after org
    :hook
    (org-mode . org-bullets-mode)
    :init
    (setq org-bullets-bullet-list '("◉" "◎" "⚫" "○" "►" "◇"))
    )

;;; Evil bindings
(use-package syndicate
    :ensure t
    :after org
    )

(use-package toc-org
  :ensure t
  :after org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook #'toc-org-enable)
  )

;;; org.el ends here
