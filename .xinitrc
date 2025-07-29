#!/bin/sh

xinput set-prop "TPPS/2 Elan TrackPoint" "libinput Accel Speed" -0.001
xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Accel Speed" 0.1
xinput set-prop "SynPS/2 Synaptics TouchPad" "libinput Natural Scrolling Enabled" 0
xset -dpms
xset dpms 0 0 0
xset s off


