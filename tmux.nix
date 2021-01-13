{ config, pkgs, lib, ... }:

{
  config = {
    user.packages = with pkgs; [ tmux ];
  };
}
