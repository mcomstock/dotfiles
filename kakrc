colorscheme color_to_the_max

def -docstring 'invoke fzf to open a file' \
    fzf-file %{ %sh{
        if [ -z "$TMUX" ]; then
            echo echo only works inside tmux
        else
            FILE=`fzf-tmux -d 15`
            if [ -n "$FILE" ]; then
                echo "eval -client '$kak_client' 'edit ${FILE}'" | kak -p ${kak_session}
            fi
        fi
}}

def -docstring 'invoke fzfc command to open a file using pre-indexed fzf' \
    fzfc-file %{ %sh{
        if [ -z "$TMUX" ]; then
            echo echo only works inside tmux
        else
            FILE=`fzfc-tmux -d 15`
            if [ -n "$FILE" ]; then
                echo "eval -client '$kak_client' 'edit ${FILE}'" | kak -p ${kak_session}
            fi
        fi
}}

def -docstring 'invoke fzf to select a buffer' \
    fzf-buffer %{ %sh{
        if [ -z "$TMUX" ]; then
            echo echo only works inside tmux
        else
            BUFFER=`echo ${kak_buflist} | tr : '\n' | fzf-tmux -d 15`
            if [ -n "$BUFFER" ]; then
                echo "eval -client '$kak_client' 'buffer ${BUFFER}'" | kak -p ${kak_session}
            fi
        fi
}}

def -docstring 'p4 open the current file' p4-open %{ echo %sh{ p4 open $kak_bufname }}
def -docstring 'p4 add the current file' p4-add %{ echo %sh{ p4 add $kak_bufname }}
def -docstring 'p4 revert the current file' p4-revert %{ echo %sh{ p4 revert $kak_bufname }}
