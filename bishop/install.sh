#!/bin/bash

HERE=$(pwd)

if >/dev/null which gcc; then
    echo "Building bishop..."
    gcc bishop.c -o bishop
    chmod +x bishop
    ln -s --force $HERE/bishop $HOME/bin/bishop
fi
