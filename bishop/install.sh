#!/bin/bash

HERE=$(pwd)

cd $HERE/files

if >/dev/null /usr/bin/which gcc; then
    echo "Building bishop..."
    gcc bishop.c -o bishop
    chmod +x bishop
    ln -s --force $HERE/files/bishop $HOME/bin/bishop
fi
