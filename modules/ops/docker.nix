{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.ops.docker;
in {
  options.modules.ops.docker = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      docker
      docker-compose
      (mkIf (config.modules.hardware.nvidia.enable) nvidia-docker)
    ];

    env.DOCKER_CONFIG = "$XDG_CONFIG_HOME/docker";
    env.MACHINE_STORAGE_PATH = "$XDG_DATA_HOME/docker/machine";

    user.extraGroups = [ "docker" ];

    # TODO
    # modules.shell.zsh.rcFiles = [ "${configDir}/docker/aliases.zsh" ]

    virtualisation = {
      docker = {
        enable = true;
        autoPrune.enable = true;
        enableOnBoot = true;
        enableNvidia = config.modules.hardware.nvidia.enable;
      };
    };
  };
}
