; **************************************************************************** ;
;                                                                              ;
;                                                         :::      ::::::::    ;
;    org.el                                             :+:      :+:    :+:    ;
;                                                     +:+ +:+         +:+      ;
;    By: mplanell <mplanell@student.42.fr>          +#+  +:+       +#+         ;
;                                                 +#+#+#+#+#+   +#+            ;
;    Created: 2018/01/19 01:59:47 by mplanell          #+#    #+#              ;
;    Updated: 2018/01/23 21:01:46 by mplanell         ###   ########.fr        ;
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
	)

(use-package org-bullets
	:ensure t
	:init
	(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
	)
