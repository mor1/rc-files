{
  description = "mort's flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
  };

  outputs =
    inputs:
    # { nixpkgs, home-manager, ... }@inputs:
    {
      nixosConfigurations = {
        greyjay = inputs.nixpkgs.lib.nixosSystem {
          specialArgs = { inherit inputs; };
          modules = [
            ./systems/greyjay
            { nixpkgs.hostPlatform = "x86_64-linux"; }
          ];
        };
      };

      homeConfigurations = {
        "mort@greyjay" = inputs.home-manager.lib.homeManagerConfiguration {
          pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
          extraSpecialArgs = { inherit inputs; };
          modules = [ ./home-manager/greyjay ];
        };

        "rmm1002@binky" = inputs.home-manager.lib.homeManagerConfiguration {
          pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
          extraSpecialArgs = { inherit inputs; };
          modules = [ ./home-manager/binky ];
        };

        "rmm1002@quoth" = inputs.home-manager.lib.homeManagerConfiguration {
          pkgs = inputs.nixpkgs.legacyPackages.x86_64-linux;
          extraSpecialArgs = { inherit inputs; };
          modules = [ ./home-manager/quoth ];
        };
      };
    };
}
