{ pkgs, ... }:
{
  home.packages =
    let
      texlive = (
        pkgs.texlive.combine {
          inherit (pkgs.texlive)
            scheme-basic # scheme-medium

            collection-fontsrecommended # font-scripts # latex-fonts
            mflua
            texlive-scripts
            xetex

            acmart
            adjustbox
            algorithm2e
            algorithmicx
            algorithms
            algpseudocodex
            amsmath
            bbding
            beamer
            biblatex
            biblatex-trad
            booktabs
            breakurl
            breqn
            caption
            cite
            cmap
            comment
            csquotes
            detex
            doublestroke
            enumitem
            environ
            epsf
            etoolbox
            everyshi
            extsizes
            fifo-stack
            filecontents
            float
            fontspec
            hyperref
            hyperxmp
            ieeetran
            ifmtarg
            ifoddpage
            jknapltx
            latexmk
            luacode
            luatexbase
            mathtools
            microtype
            multirow
            ncctools
            ninecolors
            paralist
            pdfcol
            pdflscape
            pdfpages
            pdfxup
            pgf
            pgfplots
            preprint
            relsize
            setspace
            soul
            subfig
            subfigure
            svg
            tabto-ltx
            tabularray
            tblr-extras
            tcolorbox
            texcount
            textcase
            textpos
            tikzfill
            tikzmark
            titlesec
            todonotes
            totcount
            totpages
            transparent
            ulem
            varwidth
            wrapfig
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
