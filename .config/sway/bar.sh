#!/usr/bin/env bash

# Wlan req iw

# Colors used in polybar
PRIMARY="#F0C674"
DISABLED="#707880"

echo '{"version":1}'
echo '['
echo '[],'

while true; do
    VOL_RAW=$(wpctl get-volume @DEFAULT_AUDIO_SINK@ 2>/dev/null)
    VOL_NUM=$(echo "$VOL_RAW" | awk '{print int($2*100)}')
    if echo "$VOL_RAW" | grep -q "MUTED"; then
        VOL="<span foreground='$DISABLED'>VOL muted</span>"
    else
        VOL="<span foreground='$PRIMARY'>VOL</span> ${VOL_NUM}%"
    fi

    BL_RAW=$(brightnessctl -m 2>/dev/null | cut -d, -f4)
    BL="<span foreground='$PRIMARY'>BL</span> ${BL_RAW}"


    WLAN=$(iw dev wlan0 link 2>/dev/null | grep SSID | awk '{print $2}')
    if [ -n "$WLAN" ]; then
        NET="<span foreground='$PRIMARY'>wlan0</span> $WLAN"
    else
        LAN=$(ip -o link show 2>/dev/null | awk -F': ' '{print $2}' | grep -E '^e' | head -n1)
        if [ -n "$LAN" ] && ip addr show "$LAN" 2>/dev/null | grep -q "inet "; then
            NET="<span foreground='$PRIMARY'>$LAN</span> connected"
        else
            NET="<span foreground='$DISABLED'>no net</span>"
        fi
    fi

    BAT_PERCENT=$(cat /sys/class/power_supply/BAT0/capacity 2>/dev/null)
    BAT_STATUS=$(cat /sys/class/power_supply/BAT0/status 2>/dev/null)

    if [ "$BAT_STATUS" = "Charging" ]; then
        BAT="▲ ${BAT_PERCENT}%"
    elif [ "$BAT_STATUS" = "Full" ]; then
        BAT="■ ${BAT_PERCENT}%"
    else
        BAT="▼ ${BAT_PERCENT}%"
    fi

    DATE=$(date '+%H:%M %Y-%m-%d')
    DATE="<span foreground='$PRIMARY'>$DATE</span>"

    echo "["
    echo "  {\"full_text\":\"$VOL\",\"markup\":\"pango\"},"
    echo "  {\"full_text\":\"$BL\",\"markup\":\"pango\"},"
    echo "  {\"full_text\":\"$NET\",\"markup\":\"pango\"},"
    echo "  {\"full_text\":\"$BAT\",\"markup\":\"pango\"},"
    echo "  {\"full_text\":\"$DATE\",\"markup\":\"pango\"}"
    echo "],"
    sleep 0.1
done
