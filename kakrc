#######################################
# Theme settings
#######################################

colorscheme color_to_the_max

#######################################
# Basic options
#######################################

set global tabstop 4
set global ui_options ncurses_enable_mouse=false

map global normal '/' %{/(?i)}
map global normal '<a-/>' %{<a-/>(?i)}
map global normal '?' %{?(?i)}
map global normal '<a-?>' %{<a-?>(?i)}

#######################################
# Line flags
#######################################

decl line-flags p4_diff_flags

#######################################
# Commands
#######################################

def -hidden my-kak-indent-on-new-line %{
    eval -draft -itersel %{
        # preserve previous line indent
        try %{ exec -draft \; K <a-&> }
        # cleanup trailing whitespaces from previous line
        try %{ exec -draft k <a-x> s \h+$ <ret> d }
        # indent after line ending with %[[:punct:]]
        try %{ exec -draft k <a-x> <a-k> \%[[:punct:]]$ <ret> j <a-gt> }
    }
}

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

def -docstring 'p4 diff the current file' \
    p4-diff %{ %sh{
        output=$(mktemp -d -t kak-p4.XXXXXXXX)/fifo
        mkfifo ${output}
        ( p4 diff -du $kak_buffile > ${output} 2>&1 ) > /dev/null 2>&1 < /dev/null &

        printf %s "eval -try-client '$kak_opt_docsclient' %{
            edit! -fifo ${output} *p4-diff*
            set buffer filetype 'diff'
            hook -group fifo buffer BufCloseFifo .* %{
                nop %sh{ rm -r $(dirname ${output}) }
                remove-hooks buffer fifo
            }
        }"
}}

def -docstring 'Show flags for added lines' \
    p4-line-flags %{ %sh{
        echo 'try %{ add-highlighter flag_lines GitDiffFlags p4_diff_flags }'
        p4 diff -du0 $kak_buffile | awk '
            BEGIN {
                line=0
                flags=ENVIRON["kak_timestamp"]
            }
            /^---.*/ {}
            /^@@ -[0-9]+(,[0-9]+)? \+[0-9]+(,[0-9]+)? @@.*/ {
                if ((x=index($3, ",")) > 0) {
                    line=substr($3, 2, x-2)
                } else {
                    line=substr($3, 2)
                }
            }
            /^\+/ {
                flags=flags ":" line "|{green}+"
                line++
            }
            /^\-/ { flags=flags ":" line "|{red}-" }
            END { print "set buffer p4_diff_flags ", flags }
        '
}}

def -docstring 'Run mannotate on the selected region of a file.' \
    mannotate %{ %sh{
        if [ -z "$TMUX" ]; then
            echo echo Only works inside tmux
        else
            echo "nop %sh{ ~/kak_scripts/mannotate_kak_selections.pl $kak_buffile $kak_selections_desc }"
        fi
}}

#######################################
# Hooks
#######################################

hook global WinCreate ^[^*]+$ %{
    add-highlighter line '%val{cursor_line}' default,rgb:1c1c1c
    add-highlighter show_matching
    add-highlighter number_lines -relative
}

hook global WinSetOption filetype=kak %{
    set buffer tabstop 4
    set buffer indentwidth 4
    remove-hooks window kak-indent
    hook window InsertChar \n -group kak-indent my-kak-indent-on-new-line
    hook window InsertEnd .* -group kak-indent %{ try %{ exec -draft \; <a-x> s ^\h+$ <ret> d } }
}

hook global WinSetOption filetype=perl %{
    set buffer tabstop 4
    set buffer indentwidth 0
}

hook global WinSetOption filetype=javascript %{
    set buffer tabstop 4
    set buffer indentwidth 0
}

hook global WinSetOption filetype=ruby %{
    set buffer tabstop 2
    set buffer indentwidth 2
}

#######################################
# Mode line formatting
#######################################

face PowerLineTerminator rgb:1c1c1c,black
face BufferName rgb:ffdf00,rgb:1c1c1c
face NameFileTypeSeparator rgb:444444,rgb:1c1c1c
face FileType default,rgb:444444
face FileTypeLineInfoSeparator rgb:1c1c1c,rgb:444444
face LineInfo default,rgb:1c1c1c
face LineInfoStatusInfoSeparator black,rgb:1c1c1c
face StatusInfoGap default,black
face UserInfoSeparator rgb:1c1c1c,black
face UserInfo rgb:585858,rgb:1c1c1c

#set global modelinefmt %{
#%sh{
    #if which p4 1>/dev/null; then
        #if action=$(p4 fstat -T action $kak_buffile); then
            #action=$(echo $action | awk '{print $3}')
            #echo "P4:${action}"
        #elif revision=$(p4 fstat -T haveRev $kak_buffile); then
            #revision=$(echo $revision | awk '{print $3}')
            #echo "P4:${revision}"
        #fi
    #fi
#}
#{PowerLineTerminator}{BufferName} %sh{basename $kak_bufname}
#{NameFileTypeSeparator}{FileType} %opt{filetype}
#{FileTypeLineInfoSeparator}{LineInfo} %val{cursor_line}:%val{cursor_char_column}
#{LineInfoStatusInfoSeparator}{StatusInfoGap}{{context_info}} {{mode_info}}
#{UserInfoSeparator}{UserInfo} %val{client}@[%val{session}]
#}

hook global BufCreate .*/?[^*].+ %{
    %sh{
        filename=$(basename $kak_bufname)
        buffer_fmt="{PowerLineTerminator}{BufferName} ${filename}"
        filetype_fmt='{NameFileTypeSeparator}{FileType} %opt{filetype}'
        cursor_fmt='{FileTypeLineInfoSeparator}{LineInfo} %val{cursor_line}:%val{cursor_char_column}'
        status_fmt='{LineInfoStatusInfoSeparator}{StatusInfoGap}{{context_info}} {{mode_info}}'
        user_fmt='{UserInfoSeparator}{UserInfo} %val{client}@[%val{session}]'

        fmt_line="${buffer_fmt} ${filetype_fmt} ${cursor_fmt} ${status_fmt} ${user_fmt}"
        echo "set buffer modelinefmt %{${fmt_line}}"
    }
}

#######################################
# Readline commands in insert mode
#######################################

# Shamelessly stolen from lenormf's kakoune-extra package

# move the cursor one character to the left
map global insert <c-b> <esc>hi
# move the cursor one character to the right
map global insert <c-f> <esc>li
# move the cursor one word to the left
map global insert <a-b> <esc>b\;i
# move the cursor one word to the right
map global insert <a-f> <esc>e\;i
# move the cursor to the start of the line
map global insert <c-a> <esc>I
# move the cursor to the end of the line
map global insert <c-e> <esc>gli

# delete the character under the anchor
map global insert <c-d> <esc>c
# delete from the anchor to the start of the line
map global insert <c-u> <esc>Ghc
# delete from the anchor to the end of the line
map global insert <c-k> <esc>Glc
# delete until the next word boundary
map global insert <a-d> <esc>ec
# delete until the previous word boundary
map global insert <c-w> <esc>bc

# exchange the char before cursor with the character at cursor
map global insert <c-t> %{<esc>:try 'exec H<lt>a-k>[^\n][^\n]<lt>ret>\;dp'<ret>i}
# exchange the word before cursor with the word at cursor
map global insert <a-t> %{<esc>:try 'exec <lt>a-k>\s<lt>ret>e' catch 'exec <lt>a-i>w'<ret><a-;>BS\s+<ret><a-">\;<space>i}
# uppercase the current or following word
map global insert <a-u> <esc>e<a-i>w~\;i
# lowercase the current or following word
map global insert <a-l> <esc>e<a-i>w`\;i
# capitalize the current or following word
map global insert <a-c> <esc>e<a-i>w\;~i

# paste after the anchor
map global insert <c-y> <esc>pi
