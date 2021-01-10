{ config, pkgs, ... }:
{
  services.xserver = {
    enable = true;
    layout = "us,fr";
    xkbOptions = "grp:alt_shift_toggle";
    displayManager.lightdm.enable = true;
    videoDrivers = ["nvidia"];
    libinput.enable = true;
    displayManager.defaultSession = "none+xmonad";
    windowManager = {
      xmonad = {
        enable = true;
        enableContribAndExtras = true;
      };
    };
  };
  fonts = {
    fonts = with pkgs; [
      iosevka
      merriweather
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
