{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.rofi;
in {
  options.modules.desktop.apps.rofi = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable { user.packages = with pkgs; [ rofi ]; };
}
