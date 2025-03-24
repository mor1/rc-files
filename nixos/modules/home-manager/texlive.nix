{ pkgs, ... }:
{
  home.packages =
    let
      texlive = (
        pkgs.texlive.combine {
          inherit (pkgs.texlive)
            scheme-basic # scheme-medium
            xetex

            acmart
            algorithm2e
            algorithmicx
            algorithms
            algpseudocodex
            amsmath
            biblatex
            booktabs
            breqn
            caption
            cmap
            comment
            csquotes
            enumitem
            environ
            etoolbox
            everyshi
            fifo-stack
            float
            fontspec
            hyperref
            hyperxmp
            ifmtarg
            ifoddpage
            latexmk
            luacode
            luatexbase
            microtype
            multirow
            ncctools
            pgf
            preprint
            relsize
            setspace
            subfigure
            tabto-ltx
            textcase
            textpos
            tikzmark
            todonotes
            totcount
            totpages
            ulem
            varwidth
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
