{ pkgs, ... }: {
  imports = [ ./cli.nix ./dev.nix ];

  programs = {

    bash = {
      enable = false;
      enableCompletion = false;
    };

    direnv = {
      enable = true;
      enableBashIntegration = true;
      nix-direnv.enable = true;
    };

    git = { enable = true; };
  };

  services = {
    emacs = {
      # until the addiction is kicked
      package = pkgs.emacs29;
      enable = true;
      client.enable = true;
      # defaultEditor = true;
    };
  };

}
