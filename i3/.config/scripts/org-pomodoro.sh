#!/usr/bin/env bash

export STATE=$(emacsclient --eval '(if (org-pomodoro-active-p) org-pomodoro-state -1)' 2>&1)

# Colors have to be hardcoded here. See : https://github.com/polybar/polybar/wiki/Formatting#format-tags
case "$STATE" in
    ":pomodoro")
        echo -e "%{F#B77A76}\ue003";;
    ":short-break")
        echo -e "%{F#958090}\ue005";;
    ":long-break")
        echo -e "%{F#7DAEA9}\ue006";;
    *)
        echo -e "\ue007";;
esac
