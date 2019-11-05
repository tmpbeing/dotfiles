#!/usr/bin/env bash

### Modify these variables to configure the script's behaviour

# The file where to currently used output style is saved
OUTPUT_STYLE_FILE="/tmp/pomodoro-polybar-output-style"
# How to call your emacsclient
EMACSCLIENT="emacsclient"
# If you use doom-emacs, this should be true, false otherwise
DOOM_EMACS=true
# The key used by your todo org-capture template
MY_TODO_TEMPLATE=t
# The key used by your pomodoro org-capture template
MY_POMODORO_TEMPLATE=z

### Script starts here

# If the file doesn't exist, default to SIMPLE output style
if test -e "$OUTPUT_STYLE_FILE"
then
    OUTPUT_STYLE=$(cat "$OUTPUT_STYLE_FILE")
else
    OUTPUT_STYLE="SIMPLE"
fi

prepare_and_print_output() {
    JSON_OUTPUT="{ \"text\": \"$TEXT\", \"class\": \"${STATE#:}\" }"
    echo "$JSON_OUTPUT"
}

set_state() {
    STATE=$($EMACSCLIENT --eval '(if (org-pomodoro-active-p) org-pomodoro-state -1)' 2>&1)
}

# Colors have to be hardcoded. See : https://github.com/polybar/polybar/wiki/Formatting#format-tags
set_pomodoro_icon() {
    case "$STATE" in
        ":pomodoro")
            ICON="\ue002";;
        ":overtime")
            ICON="\ue003";;
        ":short-break")
            ICON="\ue005";;
        ":long-break")
            ICON="\ue006";;
        *)
            ICON="\ue007";;
    esac
}

set_remaining_time() {
    TIME_REMAINING=$($EMACSCLIENT --eval '(org-pomodoro-format-seconds)')
    TIME_REMAINING="${TIME_REMAINING%\"}" # Bash trick to remove leading and trailing quotes
    TIME_REMAINING="${TIME_REMAINING#\"}"
}

set_task_at_hand() {
    TASK_AT_HAND=$($EMACSCLIENT --eval '(org-no-properties org-clock-current-task)')
    TASK_AT_HAND="${TASK_AT_HAND%\"}" # Bash trick to remove leading and trailing quotes
    TASK_AT_HAND="${TASK_AT_HAND#\"}"
}

# Print corresponding pomodoro icon and exit
print_pomodoro_state_simple() {
    set_state
    set_pomodoro_icon
    TEXT="$ICON"
    prepare_and_print_output
    exit
}

# Print pomodoro icon and time remaining
print_pomodoro_state_long() {
    set_state
    set_pomodoro_icon
    if [ $STATE = ":pomodoro" -o $STATE = ":short-break" -o $STATE = ":long-break" ]; then
        set_remaining_time
        TEXT="$ICON $TIME_REMAINING"
    elif [ $STATE = ":overtime" ]; then
        set_remaining_time
        TEXT="$ICON +$TIME_REMAINING"
    else # Org-pomodoro not running, nothing else to print
        TEXT="$ICON"
    fi
    prepare_and_print_output
    exit
}

print_pomodoro_state_full() {
    set_state
    set_pomodoro_icon
    if [ $STATE = ":pomodoro" ]; then
        set_remaining_time
        set_task_at_hand
        TEXT="$ICON $TIME_REMAINING $TASK_AT_HAND"
    elif [ $STATE = ":overtime" ]; then
        set_remaining_time
        set_task_at_hand
        TEXT="$ICON +$TIME_REMAINING $TASK_AT_HAND"
    elif [ $STATE = ":short-break" -o $STATE = ":long-break" ]; then #No current task to print
        set_remaining_time
        TEXT="$ICON $TIME_REMAINING"
    else # Org-pomodoro not running, nothing else to print
        TEXT="$ICON"
    fi
    prepare_and_print_output
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
    exit
fi

# Create new pomodoro item
if [ "$1" == "new" ]; then
    set_state
    if [ $STATE = ":pomodoro" -o $STATE = ":overtime" ]; then # A pomodoro is running, we don't want to start another. Just add a todo instead
        if [ $DOOM_EMACS = true ]; then
            $EMACSCLIENT --eval '(+org-capture/open-frame "" "'$MY_TODO_TEMPLATE'")'
        else
            $EMACSCLIENT -cn --frame-parameters='(quote (name . "org-capture"))' org-protocol://capture?template=$MY_TODO_TEMPLATE
        fi
    else # No pomodoro on or in a break, capture todo and start pomodoro
        if [ $DOOM_EMACS = true ]; then
            $EMACSCLIENT --eval '(+org-capture/open-frame "" "'$MY_POMODORO_TEMPLATE'")'
        else
            $EMACSCLIENT -cn --frame-parameters='(quote (name . "org-capture"))' org-protocol://capture?template=$MY_POMODORO_TEMPLATE
        fi
    fi
    # update $OUTPUT_STYLE
    exit
fi

if [ "$1" == "end-or-restart" ]; then
    set_state
    if [ $STATE = ":pomodoro" ]; then # A pomodoro is already running, do nothing
        exit
    elif [ $STATE = ":overtime" ]; then # End current pomodoro if in overtime
        $EMACSCLIENT --eval '(org-pomodoro-finished)'
    else # Call pomodoro with prefix argument to make it restart last clocked-in pomodoro
        $EMACSCLIENT --eval '(+org-pomodoro/restart-last-pomodoro)'
    fi
    # update $OUTPUT_STYLE
    exit
fi

if [ "$1" == "kill" ]; then
    set_state
    if [ $STATE = ":pomodoro" ]; then
        $EMACSCLIENT --eval '(org-pomodoro-kill)'
    fi
    # update $OUTPUT_STYLE
    exit
fi

