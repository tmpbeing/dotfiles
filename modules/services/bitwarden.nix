{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.bitwarden;
in {
  options.modules.services.bitwarden = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    services.bitwarden_rs.enable = true;

    user.extraGroups = [ "bitwarden_rs" ];

    services.fail2ban.jails.bitwarden_rs = ''
      enabled = true
      filter = bitwarden_rs
      post = 80,443,8002
      maxretry = 5
    '';

    # modules.backup.targets.bitwarden = {
    #   baseDir = "/var/lib/bitwarden_rs";
    #   owner = "bitwarden_rs";
    #   targets = [ "*" ];
    #   suspendServices = [ "bitwarden_rs" ];
    # };
  };
}
