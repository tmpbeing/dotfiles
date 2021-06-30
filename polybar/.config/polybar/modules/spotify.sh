#!/usr/bin/env bash

player_status=$(playerctl --player=spotify status 2> /dev/null)

if [ "$player_status" = "Playing" ]; then
	echo "%{F#958090} $(playerctl --player=spotify metadata --format "{{ title }} - {{ artist }}")"
elif [ "$player_status" = "Paused" ]; then
	echo "%{F#7DAEA9} $(playerctl --player=spotify metadata --format "{{ title }} - {{ artist }}")"
fi
