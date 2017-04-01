syntax enable
set background=dark
colorscheme solarized
set g:solarized_termcolors=256

set showmode
set showcmd
set wildmenu
set ruler<
runtime ftplugin/man.vim
set autoindent
set expandtab
set wrap
set hlsearch
set showmatch
set ignorecase
set smartcase
set spelllang=en_uk
nnoremap _ :set invhlsearch<CR>
set tabstop=8
set shiftwidth=4
set shiftround
let ocaml_revised = 1

" ## added by OPAM user-setup for vim / base ## 93ee63e278bdfc07d1139a748ed3fff2 ## you can edit, but keep this line
let s:opam_share_dir = system("opam config var share")
let s:opam_share_dir = substitute(s:opam_share_dir, '[\r\n]*$', '', '')

let s:opam_configuration = {}

function! OpamConfOcpIndent()
  execute "set rtp^=" . s:opam_share_dir . "/ocp-indent/vim"
endfunction
let s:opam_configuration['ocp-indent'] = function('OpamConfOcpIndent')

function! OpamConfOcpIndex()
  execute "set rtp+=" . s:opam_share_dir . "/ocp-index/vim"
endfunction
let s:opam_configuration['ocp-index'] = function('OpamConfOcpIndex')

function! OpamConfMerlin()
  let l:dir = s:opam_share_dir . "/merlin/vim"
  execute "set rtp+=" . l:dir
endfunction
let s:opam_configuration['merlin'] = function('OpamConfMerlin')

let s:opam_packages = ["ocp-indent", "ocp-index", "merlin"]
let s:opam_check_cmdline = ["opam list --installed --short --safe --color=never"] + s:opam_packages
let s:opam_available_tools = split(system(join(s:opam_check_cmdline)))
for tool in s:opam_packages
  " Respect package order (merlin should be after ocp-index)
  if count(s:opam_available_tools, tool) > 0
    call s:opam_configuration[tool]()
  endif
endfor
" ## end of OPAM user-setup addition for vim / base ## keep this line

" ## added by OPAM user-setup for vim / ocp-indent ## 8956e21a027a465f78066decc038595a ## you can edit, but keep this line
if count(s:opam_available_tools,"ocp-indent") == 0
  source "/Users/mort/.opam/system/share/vim/syntax/ocp-indent.vim"
endif
" ## end of OPAM user-setup addition for vim / ocp-indent ## keep this line
