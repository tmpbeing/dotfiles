{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.services.ssh;
in {
  options.modules.services.ssh = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    services.openssh = {
      enable = true;
      challengeResponseAuthentication = false;
      passwordAuthentication = false;
      startWhenNeeded = true;
    };

    programs.ssh.startAgent = true;

    # TODO: Add public key
    #user.openssh.authorizedKeys.keys = [""]
  };
}
