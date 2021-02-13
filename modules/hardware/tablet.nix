{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.tablet;
in
{
  options.modules.hardware.tablet = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    services.xserver.digimend.enable = true;
  };
}
