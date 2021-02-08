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
        };
        windowManager = {
          xmonad = {
            enable = true;
            enableContribAndExtras = true;
          };
        };
      };
      redshift.enable = true;
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
          xrander-sync-fence = true;
        };
      };
    };

    systemd.user.services."dunst" = {
      enable = true;
      description = "";
      wantedBy = [ "default.target" ];
      serviceConfig.Restart = "always";
      serviceConfig.RestartSec = 2;
      serviceConfig.ExecStart = "${pkgs.dunst}/bin/dunst";
    };

    home.configFile = {
      "xmonad" = {
        source = "${configDir}/xmonad";
        recursive = true;
      };
    };
  };
}
