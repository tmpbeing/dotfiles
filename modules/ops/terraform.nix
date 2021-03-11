{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.ops.terraform;
in {
  options.modules.ops.terraform = { enable = mkBoolOpt false; };

  config =
    mkIf cfg.enable { user.packages = with pkgs; [ unstable.terraform ]; };
}
