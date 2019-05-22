#!/usr/bin/env bash


OUTPUT_STYLE_FILE="/tmp/pomodoro-polybar-output-style"

# If the file doesn't exist, default to SIMPLE output style
if test -e "$OUTPUT_STYLE_FILE"
then
    OUTPUT_STYLE=$(cat "$OUTPUT_STYLE_FILE")
else
    OUTPUT_STYLE="SIMPLE"
fi

# Colors have to be hardcoded. See : https://github.com/polybar/polybar/wiki/Formatting#format-tags
set_pomodoro_icon() {
    STATE=$1
    case "$STATE" in
        ":pomodoro")
            ICON="%{F#B77A76}\ue003";;
        ":short-break")
            ICON="%{F#958090}\ue005";;
        ":long-break")
            ICON="%{F#7DAEA9}\ue006";;
        *)
            ICON="\ue007";;
    esac
}

# Print corresponding pomodoro icon and exit
print_pomodoro_state_simple() {
    export STATE=$(emacsclient --eval '(if (org-pomodoro-active-p) org-pomodoro-state -1)' 2>&1)
    set_pomodoro_icon $STATE
    echo -e "$ICON"
    exit
}

# Print pomodoro icon and time remaining
print_pomodoro_state_long() {
    export STATE=$(emacsclient --eval '(if (org-pomodoro-active-p) org-pomodoro-state -1)' 2>&1)
    set_pomodoro_icon $STATE
    if [ $STATE=":pomodoro" -o $STATE=":short-break" -o $STATE=":long-break" ]; then
        echo -e "$ICON KEK"
    else # Pomodoro not running, nothing else to print
        echo -e "$ICON"
        exit
    fi
    exit
}

update() {
    case "$1" in
        "SIMPLE")
            print_pomodoro_state_simple;;
        "LONG")
            print_pomodoro_state_long;;
        *)
            print_pomodoro_state_simple;;
    esac
}

if [ "$#" == "0" ]; then
    update $OUTPUT_STYLE
fi

# Toggle env variable to know what kind of output to show
if [ "$1" == "toggle" ]; then
    case "$OUTPUT_STYLE" in
        "SIMPLE")
            NEW_OUTPUT_STYLE="LONG";;
        "LONG")
            NEW_OUTPUT_STYLE="FULL";;
        *)
            NEW_OUTPUT_STYLE="SIMPLE";;
    esac
    echo $NEW_OUTPUT_STYLE > /tmp/pomodoro-polybar-output-style
    update $NEW_OUTPUT_STYLE
fi
