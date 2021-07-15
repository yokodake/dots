#!/usr/bin/env bash

export NIX_SHELL_PRESERVE_PROMPT=1
if [ $IN_NIX_SHELL ]; then
 export PS1='\n\[\033[1;36m\][\[\e]0;nix-shell@\h: \w\a\]nix-shell@\h:\w]\$\[\033[0m\] '
fi












# For the rest of the file:
# Copyright (C) 2015 Mirko van der Waal <mvdw at airmail dot cc>
# Distributed under terms of the GPL2 license.

# Whenever using rxvt-unicode, load the correct color file for it.
[[ -r $HOME/.Xresources && $TERM =~ 'rxvt' ]] && xrdb -all $HOME/.Xresources

# Load all the .shell files and execute (source) them.
for file in $HOME/.shell/*; do
    if [[ ! $file =~ 'ls' ]] && [[ ! -d $file ]]; then # && !directory
        source $file || . $file
    fi
done
