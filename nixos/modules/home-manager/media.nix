{ pkgs, lib, ... }:
{
  home.packages = with pkgs; [
    digikam # photo manager
    ffmpeg_7 # manipulate media files
    greg # podcast downloader
    handbrake # drive ffmpeg
    imv # image viewer
    kodi # settop box
    mediainfo # dependency of mkvtoolnix
    mkvtoolnix # manipulate MKV files, for BluRay rips
    rhythmbox # simple music playback
    vlc # video player
  ];
}
