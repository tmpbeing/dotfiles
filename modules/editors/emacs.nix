{ options, config, lib, pkgs, inputs, ... }:

with lib;
with lib.my;
let cfg = config.modules.editors.emacs;
in {
  options.modules.editors.emacs = { enable = mkBoolOpt false; };

  config = mkIf cfg.enable {
    # Activate overlay providing emacsPgtkGcc
    nixpkgs.overlays = [ inputs.emacs-overlay.overlay ];

    home.programs.emacs = {
      enable = true;
      package = pkgs.emacsPgtk;
      extraPackages = epkgs: [ epkgs.vterm ];
    };

    user.packages = with pkgs; [
      binutils # Needed for native-comp (provides 'as')
      # emacsPgtkGcc

      # Doom deps
      git
      (ripgrep.override {
        withPCRE2 = true;
      }) # Ripgrep with more powerful regex engine
      gnutls

      # Options deps
      fd # faster projectile indexing
      imagemagick # for image-dired
      (mkIf (config.programs.gnupg.agent.enable)
        pinentry_emacs) # gnupg prompts in minibuffer
      zstd # for undo-fu-session/undo-tree compression

      # Module deps
      # :checkers spell
      (aspellWithDicts (ds: with ds; [ en en-computers en-science ]))
      # :checkers grammar
      languagetool
      # :tools editorconfig
      editorconfig-core-c
      # :tools lookup & :lang org + roam
      sqlite
      # :lang latex & :lang org
      texlive.combined.scheme-medium
    ];

    env.PATH = [ "$XDG_CONFIG_HOME/emacs/bin" ];

    fonts.fonts = [ pkgs.emacs-all-the-icons-fonts ];

    # I use two daemons, one is dedicated to org-mode and the other one for everything else
    systemd.user.services."emacs-org" = {
      description = "Emacs text editor, main instance";
      serviceConfig = {
        Type = "forking";
        ExecStart =
          "${pkgs.bash}/bin/bash -c 'source ${config.system.build.setEnvironment}; exec ${pkgs.emacsPgtkGcc}/bin/emacs --daemon=org-emacs'";
        ExecStop =
          "${pkgs.emacsPgtkGcc}/bin/emacsclient --socket-name=org-emacs --eval (kill-emacs)";
        Restart = "on-failure";
      };
      wantedBy = [ "default.target" ];
    };
  };
}
