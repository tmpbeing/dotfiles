## Sources

# Powerline prompt
source ~/.shell_prompt.sh
# Fix colors
source "$HOME/.vim/bundle/gruvbox/gruvbox_256palette.sh"
# Fixing keybindings
source "$HOME/.zsh/keybindings.zsh"
# Adding clipcopy and clippaste
source "$HOME/.zsh/clipboard.zsh"
# Directories shortcuts
source "$HOME/.zsh/directories.zsh"

## Exports

# 42 Header variable
export MAIL42=mplanell@student.42.fr
export USER42=mplanell

alias free='free -m'
alias cp='cp -i'
alias zshreload="source ~/.zshrc"
alias ifconfig="ip -s -c -h a"

#GCC call
comp() { gcc -o "${1%.*}" "$1" -Wall -Wextra -Werror }
compc() { gcc -c -o "${1%.*}" "$1" -Wall -Wextra -Werror }

# List directory contents
alias l='ls -la --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias ls='ls --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias ll='ls -l --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias la='ls -la --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'

# Push and pop directories on directory stack
alias pu='pushd'
alias po='popd'

# Keybinds
# Vi mode
bindkey -v

# Syntax highlighting
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
