{ pkgs, lib, ... }:
{
  nixpkgs.config.allowUnfreePredicate =
    pkg:
    builtins.elem (lib.getName pkg) [
      "aspell-dict-en-science" # additional EN dictionary
      "corefonts" # some fonts
      "masterpdfeditor4" # edit PDF files
      "skypeforlinux" # skype, such as it is
      "slack" # slack, electron wrapper
      "vista-fonts" # vista-fonts here but vistafonts for install?!
      "zoom" # zoom here but zoom-us for install?!
    ];
}
