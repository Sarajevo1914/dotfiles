#!/usr/bin/env sh
backlight_device=$(ls -1 /sys/class/backlight 2>/dev/null | head -n 1)
echo "${backlight_device:-intel_backlight}"
