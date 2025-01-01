#!/bin/bash
# script-backlight.sh
echo $(ls -1 /sys/class/backlight | head -n 1)

