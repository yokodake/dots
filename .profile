#!/usr/bin/env bash
# Copyright (C) 2015 Mirko van der Waal <mvdw at airmail dot cc>
# Distributed under terms of the GPL2 license.

# Use a set of custom directory listing highlights.
#[[ -r $HOME/.shell/ls ]] && eval `/usr/bin/dircolors -b $HOME/.shell/ls`

# Whenever using rxvt-unicode, load the correct color file for it.
[[ -r $HOME/.Xresources && $TERM =~ 'rxvt' ]] && xrdb -all $HOME/.Xresources

# Load all the .shell files and execute (source) them.
for file in $HOME/.shell/*; do
    if [[ ! $file =~ 'ls' ]] && [[ ! -d $file ]]; then # && !directory
        source $file || . $file
    fi
done
