; **************************************************************************** ;
;                                                                              ;
;                                                         :::      ::::::::    ;
;    org.el                                             :+:      :+:    :+:    ;
;                                                     +:+ +:+         +:+      ;
;    By: mplanell <mplanell@student.42.fr>          +#+  +:+       +#+         ;
;                                                 +#+#+#+#+#+   +#+            ;
;    Created: 2018/01/19 01:59:47 by mplanell          #+#    #+#              ;
;    Updated: 2018/01/26 17:38:00 by mplanell         ###   ########.fr        ;
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
		org-agenda-start-on-weekday 1)
	)

;;; Prettier bullets
(use-package org-bullets
	:ensure t
	:after org
	:init
	(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
	)

;;; Evil bindings
(use-package syndicate
	:ensure t
	:after org
	)
