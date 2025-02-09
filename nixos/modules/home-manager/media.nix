{ pkgs, lib, ... }:
{
  home.packages = with pkgs; [
    digikam # photo manager
    ffmpeg_7 # manipulate media files
    imv # image viewer
    rhythmbox # simple music playback
    vlc # video player
  ];
}
