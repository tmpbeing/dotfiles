{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.shell.gnupg;
in
{
  options.modules.shell.gnupg = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    environment.variables.GNUPGHOME = "$XDG_CONFIG_HOME/gnupg";

    programs.gnupg.agent.enable = true;

    users.packages = with pkgs; [ tomb ];
  };
}
