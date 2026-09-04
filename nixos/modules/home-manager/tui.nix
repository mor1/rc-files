{ pkgs, ... }: {
  home.packages = with pkgs; [
    # impala # wifi mgmt # relies on iwd which is crap

    binsider # analyse and edit ELF binaries
    bluetui # (very simple) bluetooth manager
    disktui # disk partition/fs manager, depends on `parted`
    jocalsend # tui for `localsend`
    mmtui # tui for disk mount management
    wiremix # tui-based pipewire mixer
    yazi # file manager
    zoxide # smarter cd; desired by yazi
  ];
}
