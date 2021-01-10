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

  outputs = inputs @ { self, nixpkgs, nixpkgs-unstable, ... }:
    let
      system = "x86_64-linux";
      modules = [ ./configuration.nix ];


      mkPkgs = pkgs: extraOverlays: import pkgs {
        inherit system;
        config.allowUnfree = true;
        overlays = extraOverlays;
      };
      pkgs = mkPkgs nixpkgs [ self.overlay ];
      uPkgs = mkPkgs nixpkgs-unstable [];
      
    in {
      nixosConfigurations.auriga-linux = nixpkgs.lib.nixosSystem {
        inherit system;
        inherit modules;
      };
      overlay = final: prev: {
        unstable = uPkgs;
        my = self.packages."${system}";
      };
    };
}
