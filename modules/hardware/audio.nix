{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.hardware.audio;
in {
  options.modules.hardware.audio = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    sound.enable = true;
    hardware.pulseaudio.enable = true;

    user.packages = with pkgs; [ pavucontrol ];

    # HACK to prevent ~/.esd_auth from being generated. Disables esound protocol module
    hardware.pulseaudio.configFile = let
      inherit (pkgs) runCommand pulseaudio;
      paConfigFile = runCommand "disablePulseaudioEsoundModule" {
        buildInputs = [ pulseaudio ];
      } ''
        mkdir "$out"
        cp ${pulseaudio}/etc/pulse/default.pa "$out/default.pa"
        sed -i -e 's|load-module module-esound-protocol-unix|# ...|' "$out/default.pa"
      '';
    in mkIf config.hardware.pulseaudio.enable "${paConfigFile}/default.pa";

    user.extraGroups = [ "audio" ];
  };
}
