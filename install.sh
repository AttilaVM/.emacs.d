#!/bin/bash

systemCheck=true

function isPackageInstalled(){
    if ! command -v $1 >/dev/null 2>&1; then
        echo "$1 command is not avaliable on your system"
        systemCheck=false
    fi
}

function installer() {
    sudo cp $1 /usr/local/bin/
    sudo chmod +x "/usr/local/bin/$1"
}

# Install inconsolate font for the user
if ! fc-list | grep --quiet "Inconsolata"; then

    inconsolataUrl="http://www.fontsquirrel.com/fonts/download/Inconsolata"

    if ! [ -d ~/.fonts ]; then
        mkdir ~/.fonts
    fi

    cd ~/.fonts/

    wget $inconsolataUrl -O inconsolata.zip

    7z e inconsolata.zip

    fc-cache -rv

fi

# Create files if they are not exists
touch user.el control.el proxies.el

# Install bundled dependencies
bundledPackages=( "emacs-pager" )

for package in "${bundledPackages[@]}"
do
    :
    installer $package
done

# Warn to install dependencies
reaquariedPackages=( "ipython" "strace" "tern" "jscs" "tex" "gtags" )

for package in "${reaquariedPackages[@]}"
do
    :
    isPackageInstalled $package
done

if [ $systemCheck == false ];then
    echo "please install the necessery packages"
    exit
fi

echo 'Run "git config --global core.pager emacs-pager" to use Emacs for git show as a pager'
