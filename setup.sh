#!/bin/bash

# Variables
dir=~/dotfiles
olddir=~/dotfiles_old
files="emacs zshrc tmux.conf gitconfig gitignore"

# Create backup directory to avoid deleting old files
echo "Creating $olddir for backup of any existing dotfiles in ~"
mkdir -p $olddir
echo "...done"

# Move any existing files, and add symlinks
for file in $files; do
    mv ~/.$file ~/dotfiles_old/
    ln -s $dir/$file ~/.file
done

# Install plugin managers
antigen_dir=~/.zsh/antigen
tmux_dir=~/.tmux/plugins/tpm

if [ ! -d $zsh_dir ]; then
    git clone https://github.com/zsh-users/antigen $zsh_dir
fi

if [ ! -d $tmux_dir ]; then
    git clone https://github.com/tmux-plugins/tpm $tmux_dir
fi

# Install the good themes
theme_dir=~/repos/color-to-the-max-theme
emacs_themes=~/.emacs.d/color-to-the-max-theme

if [ ! -d $theme_dir ]; then
    git clone https://github.com/mcomstock/color-to-the-max-theme $theme_dir
fi

if [ ! -L $emacs_themes ]; then
    ln -s $theme_dir $emacs_themes
fi
