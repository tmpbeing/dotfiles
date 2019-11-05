#!/usr/bin/env bash

player_status=$(playerctl status 2> /dev/null)

if [ "$player_status" = "Playing" ]; then
    echo "{ \"text\":\" $(playerctl metadata --format "{{ title }} - {{ artist }}")\", \"class\": \"playing\" }"
elif [ "$player_status" = "Paused" ]; then
    echo "{ \"text\":\" $(playerctl metadata --format "{{ title }} - {{ artist }}")\", \"class\": \"paused\" }"
fi
