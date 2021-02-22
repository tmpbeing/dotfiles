# File declaring custom options to use in other modules
{ config, options, lib, home-manager, ... }:

with lib;
with lib.my; {
  options = with types; {
    user = mkOption { type = attrs; };

    home = with types; {
      file = mkOpt' attrs { } "Files to place directly in $HOME";
      configFile = mkOpt' attrs { } "Files to place in $XDG_CONFIG_HOME";
      dataFile = mkOpt' attrs { } "Files to place in $XDG_DATA_HOME";
    };

    env = mkOption {
      type = attrsOf (oneOf [ str path (listOf (either str path)) ]);
      apply = mapAttrs (n: v:
        if isList v then
          concatMapStringsSep ":" (x: toString x) v
        else
          (toString v));
      default = { };
    };
  };

  config = {
    # Usual user configuration, at top level so easily accessible
    user = {
      name = "snoop";
      # FIXME:
      password = "RMS";
      home = "/home/${config.user.name}";
      isNormalUser = true;
      extraGroups = [ "wheel" ];
      uid = 1000;
    };
    # Alias top-level user to a configured user
    users.users.${config.user.name} = mkAliasDefinitions options.user;

    home-manager = {
      useUserPackages = true;
      useGlobalPkgs = true;

      users.${config.user.name} = {
        home = {
          file = mkAliasDefinitions options.home.file;
          stateVersion = config.system.stateVersion;
        };
        xdg = {
          configFile = mkAliasDefinitions options.home.configFile;
          dataFile = mkAliasDefinitions options.home.dataFile;
        };
      };
    };

    nix = let users = [ "root" config.user.name ];
    in {
      trustedUsers = users;
      allowedUsers = users;
    };

    # TODO: Find out if/why this is needed
    env.PATH = [ "$XDG_BIN_HOME" "$PATH" ];

    # Build environment from env options
    environment.extraInit = concatStringsSep "\n"
      (mapAttrsToList (n: v: ''export ${n}="${v}"'') config.env);
  };
}
