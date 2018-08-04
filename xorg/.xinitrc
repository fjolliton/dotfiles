#!/bin/sh

hsetroot
xrandr --dpi 108 # 2560px/60cm
xmodmap ~/.xmodmap.dvorak
xcompmgr &
urxvt &
while :; do
  xmonad
  sleep 1
done
