{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.dropbox;
in {
  options.modules.services.dropbox = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [ dropbox dropbox-cli ];

    systemd.user.services."dropbox" = {
      enable = true;
      description = "Dropbox";
      wantedBy = [ "graphical-session.target" ];
      serviceConfig = {
        ExecStart = "${pkgs.dropbox.out}/bin/dropbox";
        ExecReload = "${pkgs.coreutils.out}/bin/kill -HUP $MAINPID";
        KillMode = "control-group";
        Restart = "on-failure";
        PrivateTmp = true;
        ProtectSystem = "full";
        Nice = 10;
      };
      environment = {
        DISPLAY = ":0";
      };
    };
  };
}
