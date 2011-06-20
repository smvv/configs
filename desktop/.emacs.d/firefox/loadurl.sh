#!/bin/bash

url="$1";
case $1 in
    /*)
        url="file://$1"
        ;;
esac

if "$0" -a any "openURL($url,new-tab)"; then
    echo "Loaded $url in new tab";
else
    echo "Launching Firefox...";
    firefox $url &
fi