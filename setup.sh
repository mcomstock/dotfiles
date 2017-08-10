#!/bin/bash

# Variables
dir=~/dotfiles
olddir=~/dotfiles_old
files="emacs zshrc tmux.conf gitconfig gitignore"

# Create backup directory to avoid deleting old files
echo "Creating $olddir for backup of any existing dotfiles in ~"
mkdir -p $olddir

# Move any existing files, and add symlinks
for file in $files; do
    if [ -L ~/.$file ]; then
        echo "Symbolic link for ~/.$file already exists, removing"
        rm ~/.$file
    elif [ -e ~/.$file ]; then
        echo "File ~/.$file already exists, moving to $olddir"
        mv ~/.$file ~/dotfiles_old/
    fi
    echo "Creating new symbolic link for ~/.$file"
    ln -s $dir/$file ~/.$file
done

# Install plugin managers
zsh_dir=~/.zsh/antigen
tmux_dir=~/.tmux/plugins/tpm

if [ ! -d $zsh_dir ]; then
    echo "Installing antigen"
    git clone https://github.com/zsh-users/antigen $zsh_dir
else
    echo "Skipping antigen: already installed"
fi

if [ ! -d $tmux_dir ]; then
    echo "Installing tpm"
    git clone https://github.com/tmux-plugins/tpm $tmux_dir
else
    echo "Skipping tpm: already installed"
fi

# Install the good themes
theme_dir=~/repos/color-to-the-max-theme
emacs_themes=~/.emacs.d/color-to-the-max-theme

if [ ! -d $theme_dir ]; then
    echo "Installing themes"
    git clone https://github.com/mcomstock/color-to-the-max-theme $theme_dir
else
    echo "Skipping themes: already installed"
fi

if [ ! -L $emacs_themes ]; then
    echo "Linking Emacs theme"
    ln -s $theme_dir $emacs_themes
else
    echo "Emacs theme already linked"
fi
