;;; doom-abyss-theme.el --- inspired by ayu dark
(require 'doom-themes)

;;
(defgroup doom-ayu-dark-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom doom-ayu-dark-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'doom-ayu-dark-theme
  :type 'boolean)

(defcustom doom-ayu-dark-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'doom-ayu-dark-theme
  :type 'boolean)

(defcustom doom-ayu-dark-comment-bg doom-ayu-dark-brighter-comments
  "If non-nil, comments will have a subtle, darker background. Enhancing their
legibility."
  :group 'doom-ayu-dark-theme
  :type 'boolean)

(defcustom doom-ayu-dark-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line. Can be an integer to
determine the exact padding."
  :group 'doom-ayu-dark-theme
  :type '(or integer boolean))

;;
(def-doom-theme doom-ayu-dark
  "A dark theme inspired by ayu dark"

  ;; name        default   256       16
  ((bg         '("#0a0e14" nil       nil            ))
   (bg-alt     '("#01060e" nil       nil            ))
   (base0      '("#000000" "black"   "black"        ))
   (base1      '("#070806" "#1e1e1e" "brightblack"  ))
   (base2      '("#101313" "#2e2e2e" "brightblack"  ))
   (base3      '("#181d1f" "#262626" "brightblack"  ))
   (base4      '("#283035" "#3f3f3f" "brightblack"  ))
   (base5      '("#46545e" "#525252" "brightblack"  ))
   (base6      '("#586b78" "#6b6b6b" "brightblack"  ))
   (base7      '("#6f8b91" "#979797" "brightblack"  ))
   (base8      '("#8c9595" "#dfdfdf" "white"        ))
   (fg-alt     '("#cbc6be" "#bfbfbf" "brightwhite"  ))
   (fg         '("#b3b1ad" "#2d2d2d" "white"        ))

   (grey       '("#626a73" nil       nil))
   (red        '("#f07178" "#ff6655" "red"          ))
   (orange     '("#ff8f40" "#dd8844" "brightred"    ))
   (green      '("#c2d94c" "#99bb66" "green"        ))
   (teal       '("#95e6cb" "#44b9b1" "brightgreen"  ))
   (yellow     '("#ffb454" "#ECBE7B" "yellow"       ))
   (blue       '("#39bae6" "#51afef" "brightblue"   ))
   (dark-blue  '("#59c2ff" "#2257A0" "blue"         ))
   (magenta    '("#e6b450" "#c678dd" "magenta"      ))
   (violet     '("#e6b673" "#a9a1e1" "brightmagenta"))
   (cyan       '("#f29668" "#46D9FF" "brightcyan"   ))
   (dark-cyan  '("#ffee99" "#5699AF" "cyan"   ))

   ;; face categories -- required for all themes
   (highlight      dark-blue)
   (vertical-bar   (doom-lighten base1 0.1))
   (selection      base5)
   (builtin        red)
   (comments       grey)
   (doc-comments   (doom-lighten grey 0.25))
   (constants      dark-cyan)
   (functions      yellow)
   (keywords       orange)
   (methods        yellow)
   (operators      cyan)
   (type           blue)
   (strings        green)
   (variables      fg)
   (numbers        magenta)
   (region         "#161f2a")
   (error          "#ff3333")
   (warning        yellow)
   (success        green)
   (vc-modified    "#6994bf")
   (vc-added       "#91b362")
   (vc-deleted     "#d96c75")

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright doom-ayu-dark-brighter-modeline)
   (-modeline-pad
    (when doom-ayu-dark-padded-modeline
      (if (integerp doom-ayu-dark-padded-modeline) doom-ayu-dark-padded-modeline 4)))

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

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if doom-ayu-dark-comment-bg (doom-lighten bg 0.05)))
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

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property :foreground blue)
   (css-selector :foreground yellow)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-url-face :foreground teal :weight 'normal)
   (markdown-reference-face :foreground base6)
   ((markdown-bold-face &override) :foreground fg)
   ((markdown-italic-face &override) :foreground fg-alt)

   ;; outline (affects org-mode)
   ((outline-1 &override) :foreground blue)
   ((outline-2 &override) :foreground green)
   ((outline-3 &override) :foreground (doom-darken teal 0.2))
   ((outline-4 &override) :foreground (doom-darken blue 0.2))
   ((outline-5 &override) :foreground (doom-darken green 0.2))
   ((outline-6 &override) :foreground (doom-darken teal 0.4))
   ((outline-7 &override) :foreground (doom-darken blue 0.4))
   ((outline-8 &override) :foreground (doom-darken green 0.4))

   ;; paren match
   ((paren-face-match &override) :background base5)

   ;; ivy
   ((ivy-current-match &override) :background base4)

   ;; org-mode
   (org-hide :foreground hidden)
   (org-block :background bg)
   (org-block-begin-line :background bg :foreground comments)
   (solaire-org-hide-face :foreground hidden)

   ;; slime
   (slime-early-deprecation-warning-face :strike-through warning)
   (slime-late-deprecation-warning-face :strike-through orange)
   (slime-final-deprecation-warning-face :strike-through red)
   (slime-style-warning-face :underline warning)
   (slime-warning-face :underline orange)
   (slime-error-face :underline error)
   (slime-note-face :underline dark-cyan)
   (slime-repl-inputed-output-face :foreground red)
   (sldb-restartable-frame-line-face :foreground green)

   ;; dired+
   (diredp-deletion :foreground base0 :background red)
   (diredp-deletion-file-name :foreground red)
   (diredp-flag-mark :foreground base0 :background green)
   (diredp-flag-mark-line :foreground base0 :background yellow)

   ;; ace-window
   (aw-background-face :foreground base6)
   (aw-leading-char-face :foreground red)

   ;; lispy
   (lispy-face-hint :foreground dark-blue :background base0)

   ;; override
   ((magit-header-line &override) :foreground base0)
   ((lsp-face-highlight-textual &override) :foreground base0)
   ((lsp-face-highlight-read &override) :foreground base0)
   ((lsp-face-highlight-write &override) :foreground base0)
   ((lazy-highlight &override) :foreground base0)))



;;; doom-ayu-dark-theme.el ends here
