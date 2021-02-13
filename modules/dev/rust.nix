{ options, config, lib, pkgs, ... }:

with lib;
with lib.my;
let cfg = config.modules.dev.rust;
in
{
  options.modules.dev.rust = {
    enable = mkBoolOpt false;
  };

  config = mkIf cfg.enable {
    user.packages = with pkgs; [
      rustup
      # TODO One mkIf ?
      (mkIf (config.modules.editors.emacs.enable)
        rustfmt)
      (mkIf (config.modules.editors.emacs.enable)
        unstable.rust-analyzer)
    ];

    env.RUSTUP_HOME = "$XDG_DATA_HOME/rustup";
    env.CARGO_HOME = "$XDG_DATA_HOME/cargo";
    env.PATH = [ "$CARGO_HOME/bin" ];
  };
}
