{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.desktop.media.zathura;
in {
  options.modules.desktop.media.zathura = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable { user.packages = with pkgs; [ zathura ]; };
  # TODO: Dotfiles
}
