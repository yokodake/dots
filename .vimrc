" Maintainer: Mirko van der Waal <mvdw at airmail dot cc>
"

filetype plugin indent on
syntax on

set termencoding=utf-8 encoding=utf8
set wmnu wim=list:longest,full wig=*.o,*~,*.pyc,*.tmp title titlestring=%t:%l%r%m
set titlelen=24 undofile udir=~/.vim/undo,/tmp ul=1024 ur=1024 ss=5 ls=2 nowrap
set lcs=extends:> nu nuw=1 sts=2 shiftwidth=2 ts=2 isk+=- si sta ai hid
set sc cc=80 spr sb nobk ignorecase hls magic noswapfile et tf lz
set statusline=%{Github(branch,repository)}%L:%l%=%m[%{Filesize()}]

" set tw=80 " line wrap

highlight ColorColumn ctermbg=8

" If there's a .git repository, the function will display the current directory
" name + the branch name. I'm using the directory name instead of the repository
" name because fetching the latter would be rather inefficient.
let branch = system("git branch 2>/dev/null | grep '\*'")[2:-2]
let repository = system("basename `pwd`")[:-2]

function! Github(branch, repository)
    return strlen(a:branch) == 0 ? "" : "[" . a:repository . ":" . a:branch . "] "
endfunc

function! Filesize()
    let bytes = getfsize(expand("%:p"))
    return bytes <= 1024 ? (bytes <= 0 ? "0B" : bytes . "B") : (bytes / 1024) . "K"
endfunc

augroup __cursor
    au!
    " Return to last edit position when opening files.
    au BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \   exe "normal! g`\"" |
        \ endif
augroup END

augroup __save
    au!
    " Remove the trailing whitespace whenever you're saving the file.
    au BufWritePre * :%s/\s\+$//e
    " Autosave files when focus is lost
    au FocusLost * :wa
augroup END

augroup __filtype
    au!
    " Unknown filetypes to vim, some make sense -- others don't.
    au BufRead,BufNewFile hosts         setlocal ft=conf
    au BufRead,BufNewFile *.json        setlocal ft=javascript
    au BufRead,BufNewFile *.log         setlocal ft=conf
    au BufRead,BufNewFile *.txt         setlocal ft=txt
    au BufRead,BufNewFile *.md          setlocal ft=markdown
    au BufRead,BufNewFile *.ss          setlocal ft=scheme
    au BufRead,BufNewFile *.i           setlocal ft=c
    au BufRead,BufNewFile *.h           setlocal ft=c
augroup END

