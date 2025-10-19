{
  description = "mort's flake";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager/master";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    # cmake.url = "nixpkgs/e6f23dc08d3624daab7094b701aa3954923c6bbb";
  };

  outputs =
    {
      self,
      nixpkgs,
      home-manager,
      # cmake,
      ...
    }@inputs:
    let
      system = "x86_64-linux";
      # cmake_o = final: prev: {
      #   cmake = inputs.cmake.pkg;
      # };
    in
    {
      nixosConfigurations = {
        greyjay = nixpkgs.lib.nixosSystem {
          specialArgs = { inherit inputs; };
          modules = [ ./systems/greyjay ];
        };
      };

      homeConfigurations = {
        "mort@greyjay" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          # pkgs = import nixpkgs {
          #   inherit inputs;
          #   # overlays = [ cmake_o ];
          # };

          extraSpecialArgs = { inherit inputs; };
          modules = [ ./home-manager/greyjay ];
        };

        "rmm1002@binky" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          extraSpecialArgs = { inherit inputs; };
          modules = [ ./home-manager/binky ];
        };

        "rmm1002@quoth" = home-manager.lib.homeManagerConfiguration {
          pkgs = nixpkgs.legacyPackages.x86_64-linux;
          extraSpecialArgs = { inherit inputs; };
          modules = [ ./home-manager/quoth ];
        };
      };
    };
}
