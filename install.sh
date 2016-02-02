#!/bin/bash

inconsolataUrl="http://www.fontsquirrel.com/fonts/download/Inconsolata"

if ! [ -d ~/.font ]; then
    mkdir ~/.font
fi

cd ~/.font/

wget $inconsolataUrl -O inconsolata.zip

7z e inconsolata.zip
