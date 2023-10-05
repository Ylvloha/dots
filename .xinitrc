#!/bin/sh
#
# ~/.xinitrc
#
xrdb ~/.Xresources
feh --bg-fill ~/Downloads/wallpaperbetter.com_3840x2160.jpg
picom --config ~/.config/picom.conf &
exec xmonad
