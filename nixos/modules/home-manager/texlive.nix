{ pkgs, ... }:
{
  home.packages =
    let
      texlive = (
        pkgs.texlive.combine {
          inherit (pkgs.texlive)
            # start simple...
            scheme-basic # scheme-medium
            texlive-scripts

            # add some fonts, including the University's preferred fonts
            collection-fontsrecommended # font-scripts # latex-fonts
            opensans

            # font packages
            inconsolata
            libertine
            libertinus-fonts

            # luatex-related
            lua-visual-debug
            luacode
            lualatex-math
            luatexbase
            mflua

            # xetex-related
            xetex
            xltxtra

            # add packages incrementally, for various academic paper formats,
            # tripos exam questions, etc

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
            dirtytalk
            docmute
            doublestroke
            draftwatermark
            enumitem
            environ
            epigraph
            epsf
            etoolbox
            everyshi
            extsizes
            fancyvrb
            fifo-stack
            filecontents
            float
            fontspec
            footmisc
            glossaries
            graphics
            hypdoc
            hyperref
            hyperxmp
            ieeetran
            ifmtarg
            ifoddpage
            iftex
            jknapltx
            latexmk
            listings
            lkproof
            makecell
            mathtools
            microtype
            movie15
            multirow
            natbib
            ncctools
            newtx
            nextpage
            ninecolors
            nomencl
            paralist
            pbalance
            pdfcol
            pdflscape
            pdfpages
            pdfxup
            pgf
            pgfgantt
            pgfplots
            preprint
            ragged2e
            realscripts
            refcount
            relsize
            setspace
            soul
            stackengine
            stmaryrd
            sttools
            subfig
            subfigure
            svg
            tabstackengine
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
            was
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
      ltex-ls-plus
      texlive
    ];
}
