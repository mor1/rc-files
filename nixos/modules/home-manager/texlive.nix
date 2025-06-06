{ pkgs, ... }:
{
  home.packages =
    let
      texlive = (
        pkgs.texlive.combine {
          inherit (pkgs.texlive)
            # start simple...
            scheme-basic # scheme-medium

            # add some fonts, including the University's preferred fonts
            collection-fontsrecommended # font-scripts # latex-fonts
            opensans

            # add backends and scripts
            luatexbase
            mflua
            texlive-scripts
            xetex

            # add packages incrementally, for various academic paper formats

            acmart
            acronym
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
            bigfoot
            booktabs
            breakurl
            breqn
            caption
            cite
            cleveref
            cmap
            comment
            csquotes
            detex
            doublestroke
            draftwatermark
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
            listings
            luacode
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
