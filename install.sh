#!/bin/bash

systemCheck=true

function isPackageInstalled(){
    if ! command -v $1 >/dev/null 2>&1; then
        echo "$1 command is not avaliable on your system"
        systemCheck=false
    fi
}

# Install inconsolate font for the user
inconsolataurl="http://www.fontsquirrel.com/fonts/download/Inconsolata"

if ! [ -d ~/.fonts ]; then
    mkdir ~/.fonts
fi

cd ~/.fonts/

wget $inconsolataUrl -O inconsolata.zip

7z e inconsolata.zip

fc-cache -rv

# Warn to install dependencies
reaquariedPackages=( "ipython" "strace" "tern" )

for package in "${reaquariedPackages[@]}"
do
    :
    isPackageInstalled $package
done

if [ $systemCheck == false ];then
    echo "please install the necessery packages"
    exit
fi

