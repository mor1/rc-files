{ pkgs, ... }:
{
  home.packages =
    let
      texlive = (
        pkgs.texlive.combine {
          inherit (pkgs.texlive)
            scheme-basic # scheme-medium

            acmart
            algorithm2e
            amsmath
            biblatex
            booktabs
            caption
            comment
            csquotes
            environ
            etoolbox
            everyshi
            float
            fontspec
            hyperref
            hyperxmp
            ifmtarg
            latexmk
            luacode
            luatexbase
            microtype
            ncctools
            preprint
            textcase
            textpos
            totpages
            ulem
            xcolor
            xkeyval
            xstring
            ;
        }
      );
    in
    with pkgs;
    [
      biber
      texlive
    ];
}
