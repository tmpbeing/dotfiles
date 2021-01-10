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
    zsh
    nix-zsh-completions
    bat
    exa
    fd
    fzf
  ];

  home-manager.users.snoop.xdg.enable = true;

  environment.sessionVariables = {
    # TODO: These 4 shouldn't be here
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_DATA_HOME = "$HOME/.local/share";
    XDG_BIN_HOME = "$HOME/.local/bin";
    ZDOTDIR = "$XDG_CONFIG_HOME/zsh";
    ZSH_CACHE = "$XDG_CACHE_HOME/zsh";
    ZGEN_DIR = "$XDG_DATA_HOME/zsh";
    ZGEN_SOURCE = "$ZGEN_DIR/zgen.zsh";
  };
}
