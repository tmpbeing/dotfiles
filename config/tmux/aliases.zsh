#!/usr/bin/env zsh

alias ta='tmux attack'
alias tl='tmux ls'

if [[ -n $TMUXS ]]; then
    alias tf='tmux find-window'
    alias mine='tmux detach -a'
fi
