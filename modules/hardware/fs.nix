{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.fs;
in {
  options.modules.hardware.fs = {
    enable = mkBoolOpt false;
    ssd.enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable (mkMerge [
    {
      programs.udevil.enable = true;

      environment.systemPackages = with pkgs; [
        sshfs
        exfat # Windows
        ntfs3g # Windows
        hfsprogs # MacOS
      ];
    }

    (mkIf cfg.ssd.enable { services.fstrim.enable = true; })
  ]);
}
