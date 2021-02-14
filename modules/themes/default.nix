{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.theme;
in {
  options.modules.theme = with types; {
    active = mkOption {
      type = nullOr str;
      default = null;
      apply = v:
        let theme = builtins.getEnv "THEME";
        in if theme != "" then theme else v;
      description = ''
        Name of the theme to enable.
        Can be overridden by the THEME environment variable.
      '';
    };

    wallpaper = mkOpt (either path null) null;

    loginWallpaper = mkOpt (either path null) (if cfg.wallpaper != null then
      toFilteredImage cfg.wallpaper "-gaussian-blur 0x2 -modulate 70 -level 5%"
    else
      null);

    gtk = {
      theme = mkOpt str "";
      iconTheme = mkOpt str "";
      cursorTheme = mkOpt str "";
    };

    onReload = mkOpt (attrsOf lines) { };
  };

  config = mkIf (cfg.active != null) (mkMerge [
    (let
      xrdb = ''${pkgs.xorg.xrdb}/bin/xrdb -merge "$XDG_CONFIG_HOME"/xtheme/*'';
    in {
      services.xserver.displayManager.sessionCommands = xrdb;
      modules.theme.onReload.xtheme = xrdb;
    })

    (mkIf (cfg.wallpaper != null) (let
      wCfg = config.services.xserver.desktopManager.wallpaper;
      command = ''
        if [ -e "$XDG_DATA_HOME/wallpaper" ]; then
          ${pkgs.feh}/bin/feh --bg-${wCfg.mode} \
            ${optionalString wCfg.combineScreens "--no-xinerama"} \
            --no-fehbg \
            $XDG_DATA_HOME/wallpaper
        fi
      '';
    in {
      # Set the wallpaper without leaving .fehbg
      services.xserver.displayManager.sessionCommands = command;
      modules.theme.onReload.wallpaper = command;

      home.dataFile =
        mkIf (cfg.wallpaper != null) { "wallpaper".source = cfg.wallpaper; };
    }))

    (mkIf (cfg.loginWallpaper != null) {
      services.xserver.displayManager.lightdm.background = cfg.loginWallpaper;
    })

    (mkIf (cfg.onReload != { }) (let
      reloadTheme = with pkgs;
        (writeScriptBin "reloadTheme" ''
          #!${stdenv.shell}
          echo "Reloading current theme: ${cfg.active}"
          ${concatStringsSep "\n" (mapAttrsToList (name: script: ''
            echo "[${name}]"
            ${script}
          '') cfg.onReload)}
        '');
    in {
      user.packages = [ reloadTheme ];
      system.userActivationScripts.reloadTheme = ''
        [ -z "$NORELOAD" ] && ${reloadTheme}/bin/reloadTheme
      '';
    }))
  ]);
}
