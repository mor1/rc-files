{ pkgs, ... }: {
  imports = [ ./cli.nix ./dev.nix ];

  home.packages = with pkgs; [ pkgs.emacs29-pgtk ];

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
      package = pkgs.emacs29-pgtk;
      enable = true;
      client.enable = true;
      # defaultEditor = true;
    };
  };

}
