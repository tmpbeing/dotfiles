#!/usr/bin/env bash

### Modify these variables to configure the script's behaviour

# The file where to currently used output style is saved
OUTPUT_STYLE_FILE="/tmp/pomodoro-polybar-output-style"
# How to call your emacsclient
EMACSCLIENT="emacsclient --socket=org-emacs"
# If you use doom-emacs, this should be true, false otherwise
DOOM_EMACS=true
# The key used by your todo org-capture template
MY_TODO_TEMPLATE=t
# The key used by your pomodoro org-capture template
MY_POMODORO_TEMPLATE=z

### Script starts here

if [ "$#" == "0" ]; then
    STATUS=$($EMACSCLIENT --eval '(+org-polybar/format-status-string)')
    STATUS="${STATUS%\"}" # Bash trick to remove leading and trailing quotes
    STATUS="${STATUS#\"}"
    echo -e $STATUS
fi

if [ "$1" == "toggle" ]; then
    $EMACSCLIENT --eval '(+org-polybar/clock-format-style-toggle)'
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
    $EMACSCLIENT --eval '(+org-polybar/toggle-pomodoro-or-clock)'
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
