{ pkgs, ... }: {
  home.packages = with pkgs; [
    binsider # analyse and edit ELF binaries
    bluetui # (very simple) bluetooth manager
    # impala # wifi mgmt # relies on iwd which is crap
    jocalsend # tui for `localsend`
    wiremix # tui-based pipewire mixer
    yazi # file manager
    zoxide # smarter cd; desired by yazi
  ];
}
