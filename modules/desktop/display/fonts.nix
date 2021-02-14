{ config, lib, pkgs, ... }:

{
  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      iosevka
      merriweather
      jetbrains-mono
      font-awesome-ttf
    ];
    fontconfig = {
      defaultFonts = {
        monospace = [ "Iosevka" ];
        sansSerif = [ "Iosevka" ];
        serif = [ "Merriweather" ];
      };
    };
  };
}
