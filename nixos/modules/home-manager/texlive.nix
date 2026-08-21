{ pkgs, ... }: {
  home.packages =
    let
      texlive = (
        pkgs.texliveBasic.withPackages (
          ps: with ps; [
            texlive-scripts

            # add some fonts, including the University's preferred fonts
            collection-fontsrecommended
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
            appendix
            arydshln
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
            datenumber
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
            fontawesome
            fontspec
            footmisc
            forest
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
            lineno
            lipsum
            listings
            listingsutf8
            lkproof
            llncs
            makecell
            marginnote
            mathtools
            microtype
            minted
            movie15
            multirow
            natbib
            ncctools
            newtx
            newunicodechar
            nextpage
            ninecolors
            nomencl
            paralist
            parskip
            pbalance
            pdfcol
            pdflscape
            pdfpages
            pdfxup
            pgf
            pgf-pie
            pgfgantt
            pgfplots
            placeins
            preprint
            ragged2e
            realscripts
            refcount
            relsize
            setspace
            siunitx
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
            threeparttable
            tikzfill
            tikzmark
            titlesec
            titling
            todonotes
            totcount
            totpages
            transparent
            ulem
            unicode-math
            units
            upquote
            utfsym
            varwidth
            was
            wrapfig
            xcolor
            xkeyval
            xstring
            xurl
            zref

          ]
        )
      );
    in
    with pkgs;
    [
      biber
      ltex-ls-plus
      texlive
    ];
}
