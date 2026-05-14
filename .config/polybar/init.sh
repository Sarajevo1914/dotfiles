#!/usr/bin/env bash

killall -q polybar

while pgrep -u "$UID" -x polybar >/dev/null; do sleep 1; done

LOGDIR="${XDG_STATE_HOME:-$HOME/.local/state}/polybar"
mkdir -p "$LOGDIR"

MONITORS=$(xrandr --listmonitors 2>/dev/null | awk 'NR > 1 {print $4}')

if [ -z "$MONITORS" ]; then
    echo "No monitors detected, exiting..."
    exit 1
fi

readarray -t MONITOR_ARRAY <<<"$MONITORS"

echo "--- $(date '+%Y-%m-%d %H:%M:%S.%N %z') ---" \
    | tee -a "$LOGDIR/bar1.log" "$LOGDIR/bar2.log"

if [ ${#MONITOR_ARRAY[@]} -ge 2 ]; then
    MONITOR=${MONITOR_ARRAY[0]} polybar bar1 >>"$LOGDIR/bar1.log" 2>&1 &
    MONITOR=${MONITOR_ARRAY[1]} polybar bar2 >>"$LOGDIR/bar2.log" 2>&1 &
    echo "Launched polybar on monitors: ${MONITOR_ARRAY[0]} and ${MONITOR_ARRAY[1]}"
else
    MONITOR=${MONITOR_ARRAY[0]} polybar bar1 >>"$LOGDIR/bar1.log" 2>&1 &
    echo "Launched polybar on monitor: ${MONITOR_ARRAY[0]}"
fi

echo "Bars launched..."
