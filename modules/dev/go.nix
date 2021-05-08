{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.go;
in {
  options.modules.dev.go = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      go
      go-tools
      gopls
    ];

    home.programs.go = {
      enable = true;
      goBin = ".local/bin.go";
      extraGoPaths = [ "code/go" ];
    };
  };
}
