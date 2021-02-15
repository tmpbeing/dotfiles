{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.xmonad;
in {
  options.modules.desktop.xmonad = { enable = mkBoolOpt false; };
  config = mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      dunst
      libnotify
      (polybar.override {
        pulseSupport = true;
        nlSupport = true;
      })
    ];

    services = {
      xserver = {
        enable = true;
        displayManager = {
          defaultSession = "none+xmonad";
          lightdm.enable = true;
          lightdm.greeters.mini.enable = true;
          lightdm.greeters.mini.user = config.user.name;
        };
        windowManager = {
          xmonad = {
            enable = true;
            enableContribAndExtras = true;
          };
        };
      };
      redshift = {
        enable = true;
        temperature = {
          day = 4700;
          night = 2000;
        };
      };
      picom = {
        enable = true;
        backend = "glx";
        vSync = true;
        opacityRules = [
          "100:class_g = 'VirtualBox Machine'"
          "100:class_g = 'Gimp'"
          "100:class_g = 'krita'"
          "100:class_g = 'feh'"
          "100:class_g = 'mpv'"
          "100:class_g = 'Rofi'"
          "100:class_g = 'Peek'"
          "99:_NET_WM_STATE@:32a = '_NET_WM_STATE_FULLSCREEN'"
        ];
        shadowExclude = [
          # Exclude everything but rofi and dunst
          "! name~='(rofi|Dunst)'"
        ];
        settings = {
          unredir-if-possible = true; # Might cause flicker
          glx-no-stencil = true;
          xrender-sync-fence = true;
        };
      };
    };

    systemd.user.services."dunst" = {
      enable = true;
      description = "Dunst";
      wantedBy = [ "default.target" ];
      serviceConfig = {
        Restart = "always";
        RestartSec = 2;
        ExecStart = "${pkgs.dunst}/bin/dunst";
      };
    };

    home.file = {
      ".xmonad" = {
        source = "${configDir}/xmonad";
        recursive = true;
      };
    };
  };
}
