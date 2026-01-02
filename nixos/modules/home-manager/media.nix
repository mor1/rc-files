{ pkgs, ... }:
{
  home.packages = with pkgs; [
    digikam # photo manager
    ffmpeg_7 # manipulate media files
    imv # image viewer
    musescore # music score editing
    rhythmbox
    vlc # video player
  ];
}
