#!/bin/sh

hsetroot
xrandr --dpi 108 # 2560px/60cm
xmodmap ~/.xmodmap.dvorak
xcompmgr -c -o .2 -r 20 -l -25 -t -25 &
urxvt &
while :; do
  xmonad
  sleep 1
done
