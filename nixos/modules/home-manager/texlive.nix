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
            fancyvrb
            fifo-stack
            filecontents
            float
            fontspec
            glossaries
            graphics
            hypdoc
            hyperref
            hyperxmp
            ieeetran
            ifmtarg
            ifoddpage
            iftex
            inconsolata
            jknapltx
            latexmk
            libertine
            listings
            luacode
            mathtools
            microtype
            movie15
            multirow
            natbib
            ncctools
            newtx
            ninecolors
            nomencl
            paralist
            pbalance
            pdfcol
            pdflscape
            pdfpages
            pdfxup
            pgf
            pgfplots
            preprint
            refcount
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
            unicode-math
            upquote
            varwidth
            wrapfig
            xcolor
            xkeyval
            xstring
            zref

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
