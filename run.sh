#!/bin/sh

FILENAME="./_build/default/08-2/main.exe"

while [[ 1 ]]; do 
    inotifywait -q -e close_write $FILENAME;
    sleep 0.5;
    $FILENAME; 
done
