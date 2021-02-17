{ inputs, config, lib, pkgs, ... }:

with lib;
with lib.my; {
  imports = [ # Include the results of the hardware scan.
    ./hardware-configuration.nix
    inputs.home-manager.nixosModules.home-manager
  ] ++ (mapModulesRec' (toString ./modules) import);

  nix = let
    filteredInputs = filterAttrs (n: _: n != "self") inputs;
    nixPathInputs = mapAttrsToList (n: v: "${n}=${v}") filteredInputs;
    registryInputs = mapAttrs (_: v: { flake = v; }) filteredInputs;
  in {
    package = pkgs.nixFlakes;
    extraOptions = ''
      experimental-features = nix-command flakes
    '';
    nixPath = nixPathInputs ++ [
      "nixpkgs-overlay=${dotFilesDir}/overlays"
      "dotfiles=${dotFilesDir}"
    ];
    #binaryCaches = [
    #  "https://nix-community.cachix.org"
    #]
    #binaryCachePublicKeys = [
    #]
    registry = registryInputs // { dotfiles.flake = inputs.self; };
    useSandbox = true;
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.grub = {
    enable = true;
    version = 2;
    efiSupport = true;
    enableCryptodisk = true;
    device = "nodev";
    useOSProber = true;
  };
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.luks.devices.root = {
    device = "/dev/disk/by-uuid/ab7c66b0-86e2-4d76-9525-910ea155c1c3";
    preLVM = true;
    allowDiscards = true;
  };
  boot.kernelPackages = mkDefault pkgs.linuxPackages_5_10;

  # Options to optimize SDD lifetime, supposedly.
  fileSystems."/".options = [ "noatime" "nodiratime" "discard" ];

  networking.hostName = "auriga-linux"; # Define your hostname.
  networking.wireless.enable =
    true; # Enables wireless support via wpa_supplicant.

  time.timeZone = "Europe/Paris";

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.enp39s0.useDHCP = true;
  networking.interfaces.enp42s0f1u6u4.useDHCP = true;
  networking.interfaces.wlo1.useDHCP = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    coreutils
    git
    gnumake
    wget
    vim
    unzip
  ];
  nixpkgs.config.allowUnfree = true;

  system.stateVersion = "20.09";

  # TODO: Move out to host files
  location = { # TODO: Update these
    latitude = 48.8;
    longitude = 2.3;
  };
  services.xserver = {
    xrandrHeads = [
      {
        output = "DP-0";
        primary = true;
        monitorConfig = ''
          Option "PreferredMode" "5120x1440"
          Option "Position" "0 0"
        '';
      }
      {
        output = "HDMI-0";
        monitorConfig = ''
          Option "Rotate" "Right"
          Option "LeftOf" "DP-0"
        '';
      }
    ];
  };
  modules = {
    desktop = {
      xmonad.enable = true;
      apps = {
        discord.enable = true;
        rofi.enable = true;
        slack.enable = true;
        qbittorrent.enable = true;
      };
      browsers.firefox.enable = true;
      media = {
        mpv.enable = true;
        spotify.enable = true;
        zathura.enable = true;
      };
      term.alacritty.enable = true;
    };
    dev = {
      cc.enable = true;
      cloud = {
        enable = true;
        amazon.enable = true;
      };
      elixir.enable = true;
      python.enable = true;
      rust.enable = true;
      shell.enable = true;
    };
    editors = {
      emacs.enable = true;
      vim.enable = true;
    };
    hardware = {
      audio.enable = true;
      bluetooth.enable = true;
      ergodox.enable = true;
      fs.enable = true;
      fs.ssd.enable = true;
      nvidia.enable = true;
      sensors.enable = true;
      tablet.enable = true;
    };
    services = {
      docker.enable = true;
      dropbox.enable = true;
      ssh.enable = true;
    };
    shell = {
      bitwarden.enable = true;
      direnv.enable = true;
      git.enable = true;
      gnupg.enable = true;
      tmux.enable = true;
      zsh.enable = true;
    };
    theme.active = "elly";
  };
}

