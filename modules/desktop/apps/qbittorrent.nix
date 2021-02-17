{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.apps.bittorrent;
in
{
  options.modules.desktop.apps.bittorrent = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ qbittorrent ];
  };
}
