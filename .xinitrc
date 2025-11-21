#!/bin/sh

# X11 only
[ "$XDG_SESSION_TYPE" = "x11" ] || exit 0

xinput set-prop "TPPS/2 Elan TrackPoint" "libinput Accel Speed" -0.001 2>/dev/null
xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Accel Speed" 0.1 2>/dev/null
xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Natural Scrolling Enabled" 0 2>/dev/null

xset -dpms
xset dpms 0 0 0
xset s off
