hsetroot

if [ "`hostname -s`" = blacky ]; then
    export GDK_SCALE=1.5 GDK_DPI_SCALE=1.5
    xrandr --dpi 162 # Choose the largest DPI of both monitors
fi

if [ "`hostname -s`" = mau ]; then
    xrandr --dpi 92 # 1920px/52.7cm
    dispwin -c -d1 -I .local/share/DisplayCAL/storage/HP\ 24ea\ #1\ 2019-03-14\ 11-33\ 120cdm²\ D6500\ 2.2\ F-S\ XYZLUT+MTX/HP\ 24ea\ #1\ 2019-03-14\ 11-33\ 120cdm²\ D6500\ 2.2\ F-S\ XYZLUT+MTX.icc
fi

xmodmap ~/.xmodmap.dvorak
xcompmgr -c -o .2 -r 20 -l -25 -t -25 &
urxvt &
while :; do
    xmonad
    sleep 1
done
