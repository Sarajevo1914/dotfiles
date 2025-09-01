#!/usr/bin/env bash

echo '{"version":1}'
echo '['
echo '[],'

while true; do
      VOL=$(wpctl get-volume @DEFAULT_AUDIO_SINK@ | awk '{print int($2*100)"%"}')
      BL=$(brightnessctl -m | cut -d, -f4)
      WLAN=$(iwgetid -r)
      LAN=$(ip -o link show | awk -F': ' '{print $2}' | grep -E '^e' | head -n1)
      BAT=$(cat /sys/class/power_supply/BAT0/capacity)%
      DATE=$(date '+%Y-%m-%d %X')

      if [ -n "$WLAN" ]; then
          NET="WLAN $WLAN"
      elif [ -n "$LAN" ] && ip addr show "$LAN" | grep -q "inet "; then
          NET="LAN"
      else
          NET="No net"
      fi

      echo "["
      echo "  {\"full_text\":\"$VOL\"},"
      echo "  {\"full_text\":\"$BL\"},"
      echo "  {\"full_text\":\"$NET\"},"
      echo "  {\"full_text\":\"$BAT\"},"
      echo "  {\"full_text\":\"$DATE\"}"
      echo "],"
      sleep 0.1
done
