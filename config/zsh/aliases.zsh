alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias -- -='cd -'

alias q=exit
alias clr=clear
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias mkdir='mkdir -p'

alias mk=make
alias sc=systemctl
alias scu='systemctl --user'
alias ssc='sudo systemctl'

if command -v exa >/dev/null; then
    alias ls="exa --group-directories-first"
    alias l="ls -lag"
fi
