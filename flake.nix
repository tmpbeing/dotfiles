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
      inherit (lib.my) mapModulesRec;

      system = "x86_64-linux";
      modules = [ 
        ./configuration.nix
        home-manager.nixosModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
        }
      ];


      mkPkgs = pkgs: extraOverlays: import pkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = extraOverlays;
      };
      pkgs = mkPkgs nixpkgs [ self.overlay ];
      unstable = mkPkgs nixpkgs-unstable [];
 
      lib = nixpkgs.lib.extend
        (self: super: { my = import ./lib { inherit pkgs inputs; lib = self; }; });
      
    in {
      lib = lib.my;

      nixosConfigurations.auriga-linux = nixpkgs.lib.nixosSystem {
        inherit system;
        inherit modules;
      };
      nixosModules = { dotfiles = import ./.;  } // mapModulesRec ./modules import;
      overlay = final: prev: {
        inherit unstable;
        my = self.packages."${system}";
      };
    };
}
