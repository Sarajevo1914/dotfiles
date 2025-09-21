#!/usr/bin/env sh
POLYBAR_HEIGHT=18

if pgrep -x polybar > /dev/null; then
    killall -9 polybar 2>/dev/null
    for monitor in $(bspc query -M --names); do
        bspc config -m "$monitor" top_padding 0
    done
else
    for monitor in $(bspc query -M --names); do
        bspc config -m "$monitor" top_padding "$POLYBAR_HEIGHT"
    done
    ~/.config/polybar/init.sh &
fi
