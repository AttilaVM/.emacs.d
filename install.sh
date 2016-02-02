#!/bin/bash

inconsolataUrl="http://www.fontsquirrel.com/fonts/download/Inconsolata"

if ! [ -d ~/.fonts ]; then
    mkdir ~/.fonts
fi

cd ~/.fonts/

wget $inconsolataUrl -O inconsolata.zip

7z e inconsolata.zip

fc-cache -rv
