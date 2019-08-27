;;; doom-base16-faded-theme.el
(require 'doom-themes)

;;
(defgroup doom-base16-faded-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-base16-faded-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-base16-faded-theme
  :type 'boolean)

(defcustom doom-base16-faded-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-base16-faded-theme
  :type 'boolean)

(defcustom doom-base16-faded-comment-bg doom-base16-faded-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-base16-faded-theme
  :type 'boolean)

(defcustom doom-base16-faded-padded-modeline nil
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-base16-faded-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-base16-faded
  "Doom base16-faded theme"

  ;; name        default   256       16
  ((bg         '("#2E3440" nil       nil            ))
   (bg-alt     '("#242933" nil       nil            ))
   (base0      '("#15181E" "#1C1C1C" "black"        ))
   (base1      '("#2B303B" "#303030" "brightblack"  ))
   (base2      '("#404859" "#4E4E4E" "brightblack"  ))
   (base3      '("#566176" "#626262" "brightblack"  ))
   (base4      '("#6B7994" "#808080" "brightblack"  ))
   (base5      '("#8994A9" "#949494" "brightblack"  ))
   (base6      '("#A6AEBF" "#B2B2B2" "brightblack"  ))
   (base7      '("#C4C9D4" "#D0D0D0" "brightblack"  ))
   (base8      '("#E1E4EA" "#E4E4E4" "white"        ))
   (fg-alt     '("#FFFFFF" "#FFFFFF" "brightwhite"  ))
   (fg         '("#C5C8C6" "#FFFFFF" "white"        ))

   (grey       base4)
   (red        '("#B77A76" "#AF8787" "red"          ))
   (orange     '("#C19578" "#AF8787" "brightred"    ))
   (green      '("#99A95B" "#87AF5F" "green"        ))
   (teal       '("#B4BF86" "#AFAF87" "brightgreen"  ))
   (yellow     '("#CBB079" "#D7AF87" "yellow"       ))
   (blue       '("#83A2BE" "#87AFAF" "brightblue"   ))
   (dark-blue  '("#5983A8" "#5F87AF" "blue"         ))
   (magenta    '("#796574" "#6C6C6C" "magenta"      ))
   (violet     '("#958090" "#8A8A8A" "brightmagenta"))
   (cyan       '("#7DAEA9" "#87AFAF" "brightcyan"   ))
   (dark-cyan  '("#5C938E" "#5F8787" "cyan"         ))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-lighten bg 0.05))
   (selection      dark-blue)
   (builtin        blue)
   (comments       (if doom-base16-faded-brighter-comments dark-cyan base5))
   (doc-comments   (doom-lighten (if doom-base16-faded-brighter-comments dark-cyan base5) 0.25))
   (constants      red)
   (functions      yellow)
   (keywords       blue)
   (methods        cyan)
   (operators      blue)
   (type           yellow)
   (strings        teal)
   (variables      cyan)
   (numbers        magenta)
   (region         dark-blue)
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-base16-faded-brighter-modeline)
   (-modeline-pad
    (when doom-base16-faded-padded-modeline
      (if (integerp doom-base16-faded-padded-modeline) doom-base16-faded-padded-modeline 4)))

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
    :background (if doom-base16-faded-comment-bg (doom-lighten bg 0.05)))
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

;;; doom-base16-faded-theme.el ends here
