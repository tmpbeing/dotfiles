{
  description = "My WIP flake-based nixos configuration.";

  inputs = {
    nixpkgs.url = "nixpkgs/nixos-unstable"; # primary
    nixpkgs-unstable.url = "nixpkgs/master"; # edge
    nixpkgs-stable.url = "nixpkgs/nixos-20.09"; # stable

    home-manager = {
      url = "github:rycee/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nixos-hardware.url = "github:nixos/nixos-hardware";
  };

  outputs = inputs@{ self, nixpkgs, nixpkgs-unstable, nixpkgs-stable, ... }:
    let
      inherit (lib.my) mapModules mapModulesRec;

      system = "x86_64-linux";

      mkPkgs = pkgs: extraOverlays:
        import pkgs {
          inherit system;
          config.allowUnfree = true;
          overlays = extraOverlays ++ (lib.attrValues self.overlays);
        };
      pkgs = mkPkgs nixpkgs [ self.overlay ];
      pkgsUnstable = mkPkgs nixpkgs-unstable [ ];
      pkgsStable = mkPkgs nixpkgs-stable [ ];

      lib = nixpkgs.lib.extend (self: super: {
        my = import ./lib {
          inherit pkgs inputs;
          lib = self;
        };
      });

      modules = [
        {
          nixpkgs.pkgs = pkgs;
        }
        ./.
      ];

    in {
      lib = lib.my;

      overlay = final: prev: {
        unstable = pkgsUnstable;
        stable = pkgsStable;
        my = self.packages."${system}";
      };

      overlays = mapModules ./overlays import;

      packages."${system}" = mapModules ./packages (p: pkgs.callPackage p { });

      nixosModules = {
        dotfiles = import ./.;
      } // mapModulesRec ./modules import;

      nixosConfigurations.auriga-linux = nixpkgs.lib.nixosSystem {
        inherit system;
        inherit modules;
        specialArgs = { inherit lib inputs; };
      };
    };
}
