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
            bbding
            biblatex
            booktabs
            breqn
            caption
            cmap
            comment
            csquotes
            enumitem
            environ
            epsf
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
            ninecolors
            pdflscape
            pdfpages
            pgf
            preprint
            relsize
            setspace
            soul
            subfigure
            svg
            tabto-ltx
            tabularray
            tblr-extras
            textcase
            textpos
            tikzmark
            titlesec
            todonotes
            totcount
            totpages
            transparent
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
