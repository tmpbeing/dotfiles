;;; doom-kanzo-zen-theme.el --- inspired by rebelot/kanagawa.nvim and others -*- lexical-binding: t; no-byte-compile: t; -*-
;;
;; Added: June 4 2023
;; Author: Anskrevy <https://github.com/Anskrevy
;; Maintainer:
;; Source: https://github.com/rebelot/kanagawa.nvim
;;
;;; Commentary:
;;; Original theme by rebelot see: https://github.com/rebelot/kanagawa.nvim
;;; Inspiration taken from modified version in https://github.com/NvChad/base46
;;; and konrad1977 https://github.com/konrad1977/emacs .
;;
;;; Code:

(require 'doom-themes)


;;
;;; Variables

(defgroup doom-kanzo-zen-theme nil
  "Options for the `doom-kanzo-zen' theme."
  :group 'doom-themes)

;; palette = 0=#090E13
;; palette = 1=#c4746e
;; palette = 2=#8a9a7b
;; palette = 3=#c4b28a
;; palette = 4=#8ba4b0
;; palette = 5=#a292a3
;; palette = 6=#8ea4a2
;; palette = 7=#a4a7a4
;; palette = 8=#5C6066
;; palette = 9=#e46876
;; palette = 10=#87a987
;; palette = 11=#e6c384
;; palette = 12=#7fb4ca
;; palette = 13=#938aa9
;; palette = 14=#7aa89f
;; palette = 15=#c5c9c7

;; background = #090E13
;; foreground = #c5c9c7
;; cursor-color = #c5c9c7
;; selection-background = #22262D
;; selection-foreground = #c5c9c7
;;
;;; Theme definition
;;; All grays from zed theme:
;;; #75797F
;;; #5C6066
;;; #393B44
;;; #22262D
;;; #14171D
;;;
;;; other colors not there:
;;; #b6927B ("brightwhite"
;;; #7aa89f brightcyan
;;; #8ea4a2 dimcyan
;;; #c5746e red
;;; #c34043 dimred
;;; #a292a3
;;; 
;; -- Zen Bg Shades
;; zenBg0 = "#090E13",
;; zenBg1 = "#1C1E25",
;; zenBg2 = "#22262D",
;; zenBg3 = "#393B44",
;;     -- Ink Bg Shades
;; inkBg0 = "#14171d",
;; inkBg1 = "#1f1f26",
;; inkBg2 = "#22262D",
;; inkBg3 = "#393B44",
;; inkBg4 = "#4b4e57",

;; -- Mist Bg Shades
;; mistBg0 = "#22262D",
;; mistBg1 = "#2a2c35",
;; mistBg2 = "#393B44",
;; mistBg3 = "#5C6066",
;;
;;    -- Fg and Comments
;; fg = "#C5C9C7",
;; fg2 = "#f2f1ef",
;; gray = "#717C7C",
;; gray2 = "#A4A7A4",
;; gray3 = "#909398",
;; gray4 = "#75797f",
;; gray5 = "#5C6066",


(def-doom-theme doom-kanzo-zen
    "A dark theme inspired by rebelot/kanagawa.nvim and others."

  ;; name        default   256           16
  ((bg         '("#090E13" "black"       "black"  ))
   (fg         '("#C5C9C7" "#C5C9C7"     "brightwhite"  ))

   ;; These are off-color variants of bg/fg, used primarily for `solaire-mode',
   ;; but can also be useful as a basis for subtle highlights (e.g. for hl-line
   ;; or region), especially when paired with the `doom-darken', `doom-lighten',
   ;; and `doom-blend' helper functions.
   (bg-alt     '("#1C1E25" "black"       "black"        ))
   (fg-alt     '("#f2f1ef" "#f2d1ef"     "white"        ))

   ;; These should represent a spectrum from bg to fg, where base0 is a starker
   ;; bg and base8 is a starker fg. For example, if bg is light grey and fg is
   ;; dark grey, base0 should be white and base8 should be black.
   (base0      '("#14171D" "black"       "black"        ))
   (base1      '("#1C1E25" "#1C1E25"     "brightblack"  ))
   (base2      '("#1F1F26" "#1F1F26"     "brightblack"  ))
   (base3      '("#22262D" "#22262D"     "brightblack"  ))
   (base4      '("#2a2c35" "#2A2C35"     "brightblack"  ))
   (base5      '("#393B44" "#393B44"     "brightblack"  ))
   (base6      '("#4b4e57" "#4b4e57"     "brightblack"  ))
   (base7      '("#5C6066" "#5C6066"     "brightblack"  ))
   (base8      '("#A4A7A4" "#A4A7A4"     "white"        ))

   (grey       base4)
   (red        '("#e46876" "#e46876" "red"          ))
   (orange     '("#c4746e" "#c4746e" "brightred"    ))
   (green      '("#8a9a7b" "#8a9a7b" "green"        ))
   (teal       '("#8ba4b0" "#8ba4b0" "brightgreen"  ))
   (yellow     '("#c4b28a" "#c4b28a" "yellow"       ))
   (blue       '("#7FB4CA" "#7FB4CA" "brightblue"   ))
   (dark-blue  '("#7E9CD8" "#7E9CD8" "blue"         ))
   (magenta    '("#938aa9" "#938aa9" "brightmagenta"))
   (violet     '("#9CABCA" "#9CABCA" "magenta"      ))
   (cyan       '("#A3D4D5" "#A3D4D5" "brightcyan"   ))
   (dark-cyan  '("#658594" "#658594" "cyan"         ))
   (pink       '("#A292A3" "#A292A3" "magenta"      ))
   (orange2    '("#b6927b" "#b6927b" "brightred"  ))
   (gray3      '("#909398" "#909398" "brightblack"))
   (violet2    '("#8992a7" "#8992a7" "magenta"))
   (aqua       '("#8ea4a2" "#8ea4a2" "cyan"))
   

   ;; These are the "universal syntax classes" that doom-themes establishes.
   ;; These *must* be included in every doom themes, or your theme will throw an
   ;; error, as they are used in the base theme defined in doom-themes-base.
   (highlight      fg-alt)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      (doom-darken dark-blue 0.5))
   (builtin        teal)
   (comments       base5)
   (doc-comments   (doom-lighten base5 0.25))
   (constants      orange2)
   (functions      teal)
   (keywords       violet2)
   (methods        teal)
   (operators      gray3)
   (type           aqua)
   (strings        green)
   (variables      fg)
   (numbers        pink)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)
   )


;;;; Base theme face overrides
  (

;;;; indent-guides
   (highlight-indent-guides-character-face :foreground base4)
   (highlight-indent-guides-top-character-face :foreground base4)
   (highlight-indent-guides-stack-character-face :foreground base4)
;;;; corfu
   (corfu-current :background base3)
;;;; vterm
   (vterm-color-bright-black :inherit 'term-color-bright-black :foreground base5))

;;;; Base theme variable overrides-
  ())

;;; doom-kanzo-zen-theme.el ends here
