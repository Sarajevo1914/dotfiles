#!/usr/bin/env bash

killall -q polybar

while pgrep -u $UID -x polybar >/dev/null; do sleep 1; done

MONITORS=$(xrandr --listmonitors 2>/dev/null | grep -v "Monitors" | awk '{print $4}')

if [ -z "$MONITORS" ]; then
    echo "No monitors detected, exiting..."
    exit 1
fi

MONITOR_ARRAY=($MONITORS)

echo "---" | tee -a /tmp/polybar1.log /tmp/polybar2.log

if [ ${#MONITOR_ARRAY[@]} -ge 2 ]; then
    MONITOR=${MONITOR_ARRAY[0]} polybar bar1 2>&1 | tee -a /tmp/polybar1.log & disown
    MONITOR=${MONITOR_ARRAY[1]} polybar bar2 2>&1 | tee -a /tmp/polybar2.log & disown
    echo "Launched polybar on monitors: ${MONITOR_ARRAY[0]} and ${MONITOR_ARRAY[1]}"
else
    MONITOR=${MONITOR_ARRAY[0]} polybar bar1 2>&1 | tee -a /tmp/polybar1.log & disown
    echo "Launched polybar on monitor: ${MONITOR_ARRAY[0]}"
fi

echo "Bars launched..."
