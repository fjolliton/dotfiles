#!/usr/bin/env bash

hsetroot
if [[ `hostname -s` = blacky ]]; then
    export GDK_SCALE=1.5 GDK_DPI_SCALE=1.5
    xrandr --dpi 162 # Choose the largest DPI of both monitors
fi
xmodmap ~/.xmodmap.dvorak
xcompmgr -c -o .2 -r 20 -l -25 -t -25 &
urxvt &
while :; do
    xmonad
    sleep 1
done
