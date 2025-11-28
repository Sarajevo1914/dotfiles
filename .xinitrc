#!/bin/sh

# X11 only
[ "$XDG_SESSION_TYPE" = "x11" ] || exit 0

# TrackPoint
xinput set-prop "TPPS/2 Elan TrackPoint" "libinput Accel Speed" 0.3 2>/dev/null
xinput set-prop "TPPS/2 Elan TrackPoint" "libinput Accel Profile Enabled" 0 1 0 2>/dev/null

# TouchPad
xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Accel Speed" 0.3 2>/dev/null
xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Natural Scrolling Enabled" 0 2>/dev/null
xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Disable While Typing Enabled" 0 2>/dev/null

# X11 power settings
xset -dpms
xset dpms 0 0 0
xset s off
