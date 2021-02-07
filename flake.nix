{
  description = "My WIP flake-based nixos configuration.";

  inputs =
    {
      nixpkgs.url = "nixpkgs/master";
      nixpkgs-unstable.url = "nixpkgs/master";

      home-manager = {
        url = "github:rycee/home-manager/master";
        inputs.nixpkgs.follows = "nixpkgs";
      };

      emacs-overlay.url = "github:nix-community/emacs-overlay";
      nixos-hardware.url = "github:nixos/nixos-hardware";
    };

  outputs = inputs @ { self, nixpkgs, nixpkgs-unstable, home-manager, ... }:
    let
      inherit (lib.my) mapModules mapModulesRec;

      system = "x86_64-linux";
      modules = [
          ./.
	];


      mkPkgs = pkgs: extraOverlays: import pkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = extraOverlays ++ (lib.attrValues self.overlays);
      };
      pkgs = mkPkgs nixpkgs [ self.overlay ];
      pkgs' = mkPkgs nixpkgs-unstable [];
 
      lib = nixpkgs.lib.extend
        (self: super: { my = import ./lib { inherit pkgs inputs; lib = self; }; });
      
    in {
      lib = lib.my;

      overlay = final: prev: {
        unstable = pkgs';
        my = self.packages."${system}";
      };

      overlays = mapModules ./overlays import;

      packages."${system}" = mapModules ./packages (p: pkgs.callPackage p {});

      nixosModules = { dotfiles = import ./.;  } // mapModulesRec ./modules import;

      nixosConfigurations.auriga-linux = nixpkgs.lib.nixosSystem {
        inherit system;
        inherit modules;
        specialArgs = { inherit lib inputs; };
      };
    };
}
