;;; doom-elly-theme.el
(require 'doom-themes)

;;
(defgroup doom-elly-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-elly-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-elly-theme
  :type 'boolean)

(defcustom doom-elly-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-elly-theme
  :type 'boolean)

(defcustom doom-elly-comment-bg doom-elly-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-elly-theme
  :type 'boolean)

(defcustom doom-elly-padded-modeline nil
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-elly-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-elly
  "Doom elly theme"

  ;; name        default   256       16
  ((bg         '("#111A1F" nil       nil            ))
   (bg-alt     '("#21333D" nil       nil            ))
   (base0      '("#14191F" "#121212" "black"        ))
   (base1      '("#151A1E" "#262626" "brightblack"  ))
   (base2      '("#253340" "#3A3A3A" "brightblack"  ))
   (base3      '("#2D3640" "#4E4E4E" "brightblack"  ))
   (base4      '("#3E4B59" "#626262" "brightblack"  ))
   (base5      '("#5A666B" "#626262" "brightblack"  ))
   (base6      '("#6C7A80" "#767676" "brightblack"  ))
   (base7      '("#808E94" "#8A8A8A" "brightblack"  ))
   (base8      '("#95A1A6" "#9E9E9E" "brightblack"  ))
   (fg-alt     '("#CDD2D5" "#D0D0D0" "brightwhite"  ))
   (fg         '("#ACB3B5" "#C6C6C6" "white"        ))

   (grey       base5)
   (red        '("#8D7856" "#87875F" "red"          ))
   (orange     '("#948556" "#87875F" "brightred"    ))
   (green      '("#61694E" "#5F5F5F" "green"        ))
   (teal       '("#798362" "#87875F" "brightgreen"  ))
   (yellow     '("#9B9257" "#87875F" "yellow"       ))
   (blue       '("#63768A" "#5F8787" "brightblue"   ))
   (dark-blue  '("#4F5E6E" "#5F5F5F" "blue"         ))
   (magenta    '("#5A717F" "#5F5F87" "magenta"      ))
   (violet     '("#738C9C" "#5F87AF" "brightmagenta"))
   (cyan       '("#6998B3" "#5F87AF" "brightcyan"   ))
   (dark-cyan  '("#4C7C97" "#5F8787" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-lighten bg 0.05))
   (selection      base2)
   (builtin        red)
   (comments       (if doom-elly-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if doom-elly-brighter-comments dark-cyan base5) 0.25))
   (constants      blue)
   (functions      red)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        teal)
   (variables      cyan)
   (numbers        magenta)
   (region         base3)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-elly-brighter-modeline)
   (-modeline-pad
    (when doom-elly-padded-modeline
      (if (integerp doom-elly-padded-modeline) doom-elly-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

   (modeline-bg
    (if -modeline-bright
        base3
        `(,(doom-darken (car bg) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        base3
        `(,(doom-darken (car bg) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   (doom-darken bg 0.1))
   (modeline-bg-inactive-l `(,(car bg) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ((line-number &override) :foreground fg-alt)
   ((line-number-current-line &override) :foreground fg)
   ((line-number &override) :background (doom-darken bg 0.025))

   (font-lock-comment-face
    :foreground comments
    :background (if doom-elly-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))
   (mode-line-buffer-id
    :foreground highlight)

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   (telephone-line-accent-active
    :inherit 'mode-line
    :background (doom-lighten bg 0.2))
   (telephone-line-accent-inactive
    :inherit 'mode-line
    :background (doom-lighten bg 0.05))
   (telephone-line-evil-emacs
    :inherit 'mode-line
    :background dark-blue)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-code-face :background (doom-lighten base3 0.05))

   ;; org-mode
   (org-hide :foreground hidden)
   (org-block :background base2)
   (org-block-begin-line :background base2 :foreground comments)
   (solaire-org-hide-face :foreground hidden))


  ;; --- extra variables ---------------------
  ;; ()
  )

;;; doom-elly-theme.el ends here
