{ config, lib, pkgs, inputs, ...}:

with lib;
{
  config = {
    # Activate overlay providing emacsPgtkGcc
    nixpkgs.overlays = [ inputs.emacs-overlay.overlay ];

    user.packages = with pkgs; [
      binutils # Needed for native-comp (provides 'as')
      emacsPgtkGccDoesNotExist

      git
      (ripgrep.override {withPCRE2 = true;}) # Ripgrep with more powerful regex engine
      gnutls
     
      fd # faster projectile indexing
      imagemagick # for image-dired
      (mkIf (config.programs.gnupg.agent.enable)
        pinentry_emacs) # gnupg prompts in minibuffer
      zstd # for undo-fu-session/undo-tree compression
    ];

    # TODO: Uncomment when properly setup. Adds doom bin to the path
    # env.PATH = [ "$XDG_CONFIG_HOME/emacs/bin" ];

    fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];
  };
}
