-- | My XMonad config

-- Base
import           XMonad
import           System.Exit                    ( exitSuccess )
import qualified XMonad.StackSet               as W

-- Actions
import           XMonad.Actions.CopyWindow
import           XMonad.Actions.CycleWS
import           XMonad.Actions.DynamicProjects
-- TODO: Add bindings for it
import           XMonad.Actions.GroupNavigation
import           XMonad.Actions.MouseResize
import           XMonad.Actions.Promote
import           XMonad.Layout.Renamed
import           XMonad.Actions.RotSlaves
import           XMonad.Actions.WithAll


-- Config
import           XMonad.Config.Desktop

-- Hooks
import           XMonad.Hooks.EwmhDesktops
import           XMonad.Hooks.FadeInactive
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.ManageHelpers
import           XMonad.Hooks.Place
import           XMonad.Hooks.SetWMName


-- Layout
import           XMonad.Layout.Fullscreen       ( fullscreenSupport
                                                , fullscreenManageHook
                                                , fullscreenFull
                                                )
import           XMonad.Layout.ResizableTile
import           XMonad.Layout.ThreeColumns
import           XMonad.Layout.TwoPanePersistent

-- Layout modifiers
import           XMonad.Layout.IfMax            ( IfMax(..) )
import           XMonad.Layout.LayoutHints
import           XMonad.Layout.LayoutModifier
import           XMonad.Layout.NoBorders
import           XMonad.Layout.Spacing
import           XMonad.Layout.ToggleLayouts
import           XMonad.Layout.WindowArranger   ( windowArrange
                                                , WindowArrangerMsg(..)
                                                )


-- Utilities
import           XMonad.Util.EZConfig
import           XMonad.Util.SpawnOnce

main = xmonad myConfig

myConfig =
  dynamicProjects projects
    $                 fullscreenSupport
    $                 ewmh
    $ desktopConfig { borderWidth        = myBorderWidth
                    , focusFollowsMouse  = myFocusFollowsMouse
                    , focusedBorderColor = myFocusColor
                    , handleEventHook = docksEventHook <+> fullscreenEventHook
                    , layoutHook         = myLayoutHook
                    , logHook            = myLogHook
                    , manageHook         = myManageHook
                    , modMask            = myModMask
                    , normalBorderColor  = myNormColor
                    , startupHook        = myStartupHook <+> ewmhDesktopsStartup
                    , terminal           = myTerminal
                    , workspaces         = myWorkspaces
                    }
    `additionalKeysP` myKeys


myFont :: String
myFont = "xft:Iosevka:size=12"

myTerminal :: String
myTerminal = "alacritty"

myEditor :: String
myEditor =
  "emacsclient --frame-parameters='(quote (name . \"main-emacs\"))' -nc"

myModMask :: KeyMask
myModMask = mod4Mask

myWorkspaces :: [String]
myWorkspaces = ["main", "chat", "media", "org", "misc", "trash"]

myBorderWidth :: Dimension
myBorderWidth = 2

myNormColor :: String
myNormColor = "#798362"

myFocusColor :: String
myFocusColor = "#8d7856"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = True

myLogHook :: X ()
myLogHook =
  historyHook <+> ewmhDesktopsLogHook <+> fadeInactiveLogHook fadeAmount
  where fadeAmount = 0.8

myStartupHook :: X ()
myStartupHook = do
  spawn "/home/snoop/.config/polybar/polybar-handler"
  setWMName "LG3D"


mySpacing
  :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

myLayoutHook = layoutHints . avoidStruts . IfMax 1 single_window $ layouts
 where
  single_window = renamed [Replace "Single"] $ noBorders $ avoidStruts $ Full
  full = renamed [Replace "Fullscreen"] $ noBorders (fullscreenFull Full)
  twoPanes = renamed [Replace "Two Panes"] $ mySpacing 8 $ TwoPanePersistent
    Nothing
    (3 / 100)
    (1 / 2)
  threeColMid =
    renamed [Replace "Three Columns Mid"] $ mySpacing 8 $ ThreeColMid
      1
      (3 / 100)
      (60 / 100)
  tall = renamed [Replace "Tall"] $ mySpacing 8 $ ResizableTall 1
                                                                (2 / 100)
                                                                (1 / 2)
                                                                []
  layouts = -- tall |||
    twoPanes ||| threeColMid ||| full
    -- ||| single_window

myManageHook =
  composeAll
      [ className =? "firefox" --> doShift "main"
      , className =? "slack" --> doShift "chat"
      , className =? "spotify" --> doShift "media"
      , className =? "qBittorrent" --> doShift "misc"
      , title =? "main-emacs" --> doShift "main"
      , title =? "doom-capture" --> doFloat
      ]
    <+> placeHook simpleSmart
    <+> manageDocks
    <+> (isFullscreen --> doFullFloat)


-- Using EzConfig syntax
myKeys :: [(String, X ())]
myKeys =
    -- Xmonad
  [ ( "M-C-r"
    , spawn "xmonad --recompile"
    )        -- Recompiles xmonad
  , ( "M-S-r"
    , spawn "xmonad --restart"
    )        -- Restarts xmonad
  , ( "M-S-e"
    , io exitSuccess
    )        -- Quits xmonads

    -- Start programs
  , ( "M-<Return>"
    , spawn myTerminal
    ) -- Spawn the configured terminal emulator
  , ("M-d"  , spawn "rofi -matching fuzzy -modi 'run#ssh#window' -show run")
  , ("M-S-d", spawn "rofi -matching fuzzy -modi 'run#ssh#window' -show ssh")
  , ("M-S-w", spawn "rofi -matching fuzzy -modi 'run#ssh#window' -show window")
  , ( "M-n"
    , spawn "~/.config/doom/org-capture"
    ) -- Emacs org-capture

    -- Windows
  , ( "M-q"
    , kill1
    ) -- Kill the currenly focused window
  , ( "M-S-q"
    , killAll
    ) -- Kill all windows in the current workspace
  , ( "M-m"
    , windows W.focusMaster
    )     -- Move focus to the master window
  , ( "M-j"
    , windows W.focusDown
    )       -- Move focus to the next window
  , ( "M-k"
    , windows W.focusUp
    )         -- Move focus to the prev window
  , ( "M-S-j"
    , windows W.swapDown
    )      -- Swap focused window with next window
  , ( "M-S-k"
    , windows W.swapUp
    )        -- Swap focused window with prev window
  , ( "M-<Backspace>"
    , promote
    )         -- Moves focused window to master, others maintain order
  , ( "M1-S-<Tab>"
    , rotSlavesDown
    )      -- Rotate all windows except master and keep focus in place
  , ( "M1-C-<Tab>"
    , rotAllDown
    )         -- Rotate all the windows in the current stack
  , ( "M-S-s"
    , windows copyToAll
    )       -- Keep window up on all workspaces
  , ( "M-C-s"
    , killAllOtherCopies
    )      -- Restore window behaviour from copyToAll

    -- Layouts
  , ( "M-<Tab>"
    , sendMessage NextLayout
    )               -- Switch to next layout
        -- , ("M-C-M1-<Up>", sendMessage Arrange)              -- Float window
        -- , ("M-C-M1-<Down>", sendMessage DeArrange)
  , ( "M-f"
    , sendMessage (Toggle "Fullscreen") >> sendMessage ToggleStruts
    ) -- Toggles noborder/full

      -- Workspaces
  , ( "M-."
    , nextScreen
    )  -- Switch focus to next monitor
  , ("M-,", prevScreen)  -- Switch focus to prev monitor
  ]


projects :: [Project]
projects =
  [ Project
    { projectName      = "main"
    , projectDirectory = "~/"
    , projectStartHook = Just $ do
                           sendMessage (Toggle "Three Columns Mid")
                           spawn "emacsclient -nc --socket=main-emacs"
                           spawn "firefox"
                           spawn "alacritty -e tmux new-session -A -s main"
    }
  , Project
    { projectName      = "chat"
    , projectDirectory = "~/"
    , projectStartHook = Just $ do
                           sendMessage (Toggle "Two Panes")
                           spawn "slack"
                           spawn "Discord"
    }
  , Project
    { projectName      = "media"
    , projectDirectory = "~/"
    , projectStartHook = Just $ do
                           spawn "spotify"
    }
  ,
    -- TODO : Add parameters to emacsclient to differenciate frame
    Project
    { projectName      = "org"
    , projectDirectory = "~/Dropbox/org"
    , projectStartHook = Just $ do
      spawn
        "emacsclient -nc --socket=org-emacs --eval '(+org/open-org-overview)'"
    }
  ]
