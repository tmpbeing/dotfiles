{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.database;
in {
  options.modules.dev.database = { enable = mkBoolOpt false; };

  config =
    mkIf cfg.enable { user.packages = with pkgs; [ dbeaver postgresql ]; };
}
