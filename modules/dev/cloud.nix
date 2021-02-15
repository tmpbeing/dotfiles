{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.cloud;
in {
  options.modules.dev.cloud = {
    enable = mkBoolOpt false;
    google.enable = mkBoolOpt false;
    amazon.enable = mkBoolOpt false;
    microsoft.enable = mkBoolOpt false;
  };

  config = mkMerge [
    (mkIf cfg.amazon.enable { user.packages = with pkgs; [ awscli ]; })
    (mkIf cfg.google.enable {
      user.packages = with pkgs; [google-cloud-sdk cloud-sql-proxy];
    })
    (mkIf cfg.microsoft.enable { user.packages = with pkgs; [ azure-cli ]; })

    (mkIf cfg.enable {
      user = {
        packages = with pkgs; [
          unstable.terraform
          unstable.kubectl
          unstable.kubernetes-helm
        ];
      };
    })
  ];
}
