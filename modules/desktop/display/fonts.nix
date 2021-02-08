{ config, lib, pkgs, ... }:

{
  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      iosevka
      merriweather
      jetbrains-mono
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
