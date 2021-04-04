{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.ops.kubernetes;
in {
  options.modules.ops.kubernetes = {
    enable = mkBoolOpt false;

  };
  config = mkMerge [
    (mkIf cfg.enable {
      user.packages = with pkgs; [
        unstable.kubectl
        unstable.kubernetes-helm
        unstable.kubectx
        unstable.stern
        unstable.lens
      ];
    })
    (mkIf config.modules.ops.cloud.enable {
      user.packages = with pkgs; [
        nodejs # For pulumi
        nodePackages.typescript
        nodePackages.prettier
        unstable.pulumi-bin
      ];
    })
  ];
}
