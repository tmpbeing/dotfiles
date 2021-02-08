{ config, pkgs, ... }:
{
  services.xserver = {
    layout = "us,fr";
    xkbOptions = "grp:alt_shift_toggle";
    videoDrivers = ["nvidia"];
    libinput.enable = true;
  };
}
