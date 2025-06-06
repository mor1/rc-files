{ pkgs, lib, ... }:
let
  cursor = {
    name = "Bibata-Modern-Ice";
    package = pkgs.bibata-cursors;
    size = 20;
  };
in
{
  wayland.windowManager.sway.config = {
    seat."*".xcursor_theme = "${cursor.name} ${toString cursor.size}";
  };

  home.pointerCursor = {
    name = "${cursor.name}";
    size = cursor.size;
    package = cursor.package;
    gtk.enable = true;
    x11 = {
      enable = true;
      defaultCursor = "wayland-cursor";
    };
  };

  gtk = {
    enable = true;
    cursorTheme = {
      name = "${cursor.name}";
      package = cursor.package;
      size = cursor.size;
    };
  };

}
