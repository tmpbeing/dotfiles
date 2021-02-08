{ config, pkgs, ... }:

{
  users.defaultUserShell = pkgs.zsh;

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    # Set to false and init myself is some commands don't have completions
    enableGlobalCompInit = true;
    promptInit = "";
  };

  # TODO: How to install for user ?
  environment.systemPackages = with pkgs; [
    # TODO: Move alacritty out
    alacritty
    zsh
    nix-zsh-completions
    bat
    exa
    fd
    fzf
  ];

  environment.sessionVariables = {
    ZDOTDIR = "$XDG_CONFIG_HOME/zsh";
    ZSH_CACHE = "$XDG_CACHE_HOME/zsh";
    ZGEN_DIR = "$XDG_DATA_HOME/zsh";
    ZGEN_SOURCE = "$ZGEN_DIR/zgen.zsh";
  };
}
