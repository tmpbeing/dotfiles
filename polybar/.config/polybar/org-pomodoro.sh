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
set_state_and_pomodoro_icon() {
    STATE=$(emacsclient --eval '(if (org-pomodoro-active-p) org-pomodoro-state -1)' 2>&1)
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

set_remaining_time() {
    TIME_REMAINING=$(emacsclient --eval '(org-pomodoro-format-seconds)')
    TIME_REMAINING="${TIME_REMAINING%\"}" # Bash trick to remove leading and trailing quotes
    TIME_REMAINING="${TIME_REMAINING#\"}"
}

set_task_at_hand() {
    TASK_AT_HAND=$(emacsclient --eval '(org-no-properties org-clock-current-task)')
    TASK_AT_HAND="${TASK_AT_HAND%\"}" # Bash trick to remove leading and trailing quotes
    TASK_AT_HAND="${TASK_AT_HAND#\"}"
}

# Print corresponding pomodoro icon and exit
print_pomodoro_state_simple() {
    set_state_and_pomodoro_icon
    echo -e "$ICON"
    exit
}

# Print pomodoro icon and time remaining
print_pomodoro_state_long() {
    set_state_and_pomodoro_icon
    if [ $STATE = ":pomodoro" -o $STATE = ":short-break" -o $STATE = ":long-break" ]; then
        set_remaining_time
        echo -e "$ICON $TIME_REMAINING"
    else # Org-pomodoro not running, nothing else to print
        echo -e "$ICON"
        exit
    fi
    exit
}

print_pomodoro_state_full() {
    set_state_and_pomodoro_icon
    if [ $STATE = ":pomodoro" ]; then
        set_remaining_time
        set_task_at_hand
        echo -e "$ICON $TIME_REMAINING $TASK_AT_HAND"
    elif [ $STATE = ":short-break" -o $STATE = ":long-break" ]; then #No current task to print
        set_remaining_time
        echo -e "$ICON $TIME_REMAINING"
    else # Org-pomodoro not running, nothing else to print
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
            print_pomodoro_state_full;;
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
    # For now echoing the output on click does not update the bar so this would be pointless
    # Keep an eye on : https://github.com/polybar/polybar/issues/720
    # update $NEW_OUTPUT_STYLE
fi
