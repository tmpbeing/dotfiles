{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.term.alacritty;
in {
  options.modules.desktop.term.alacritty = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ alacritty ];

    home.configFile = {
      "alacritty/alacritty.yml".source = "${configDir}/alacritty/alacritty.yml";
    };

    # https://github.com/alacritty/alacritty/issues/4707
    env.WINIT_X11_SCALE_FACTOR = "1";
  };
}
