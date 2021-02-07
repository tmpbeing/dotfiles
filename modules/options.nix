# File declaring custom options to use in other modules
{ config, options, lib, home-manager, ... }:

with lib;
{
  options = with types; {
    user = mkOption { type = attrs; };

    env = mkOption {
      type = attrsOf (oneOf [ str path (listOf (either str path)) ]); 
      apply = mapAttrs
        (n: v: if isList v
               then concatMapStringsSep ":" (x: toString x) v
               else (toString v));
      default = {};
    };
  };

  config = {
    # Usual user configuration, at top level so easily accessible
    user = {
      name = "snoop";
      password = "RMS";
      isNormalUser = true;
      extraGroups = [ "wheel" "docker" ];
      uid = 1000;
    };
    # Alias top-level user to a configured user
    users.users.${config.user.name} = mkAliasDefinitions options.user;

    home-manager = {
      useUserPackages = true;
    }

    nix = let users = [ "root" config.user.name ]; in {
      trustedUsers = users;
      allowedUsers = users;
    };

    # TODO: Find out if/why this is needed
    env.PATH = [ "$PATH" ];
  
    # Build environment from env options
    environment.extraInit =
      concatStringsSep "\n"
        (mapAttrsToList (n: v: "export ${n}=\"${v}\"") config.env);
    };
}
