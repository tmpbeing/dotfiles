{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.theme;
in {
  config = mkIf (cfg.active == "elly") (mkMerge [
    {
      modules = {
        theme = {
          wallpaper = mkDefault ./config/wallpaper.jpg;
          gtk = {
            # TODO: theme = "?";
            iconTheme = "Paper";
            cursorTheme = "Paper";
          };
        };

        shell.tmux.rcFiles = [ ./config/tmux.conf ];
        desktop.browsers = {
          firefox.userChrome = concatMapStringsSep "\n" readFile
            [ ./config/firefox/userChrome.css ];
        };
      };
    }

    (mkIf config.services.xserver.enable {
      user.packages = with pkgs; [ paper-icon-theme ];

      services.picom = {
        fade = true;
        fadeDelta = 1;
        fadeSteps = [ 1.0e-2 1.2e-2 ];
        shadow = true;
        shadowOffsets = [ (-10) (-10) ];
        shadowOpacity = 0.22;
        settings = {
          shadow-radius = 12;
          blur-kern = "7x7box";
          blur-strength = 320;
        };
      };

      services.xserver.displayManager.lightdm.greeters.mini.extraConfig = ''
        text-color =  #acb3b5
        password-background-color = #111a1f
        window-color = #868b8d
        border-color = #798362
      '';

      home.configFile = with config.modules;
        mkMerge [
          { "xtheme/90-theme".source = ./config/Xresources; }
          (mkIf desktop.xmonad.enable {
            "polybar" = {
              source = ./config/polybar;
              recursive = true;
            };
            "dunst/dunstrc".source = ./config/dunstrc;
          })
          (mkIf desktop.apps.rofi.enable {
            "rofi" = {
              source = ./config/rofi;
              recursive = true;
            };
          })
        ];
    })
  ]);
}
