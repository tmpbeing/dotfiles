# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    .zshrc                                             :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: mplanell <mplanell@student.42.fr>          +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2016/08/14 13:32:43 by mplanell          #+#    #+#              #
#    Updated: 2016/12/17 15:21:46 by mplanell         ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

# If not running interactively, do nothing
case $- in
	*i*) ;;
	  *) return ;;
esac

## Exports

# 42 Header variable
export MAIL42=mplanell@student.42.fr
export USER42=mplanell

# Other
export EDITOR="vim"

## Aliases

# Convenience
alias c='clear'
alias h='history'
alias update='pacaur -Syu'
alias free='free -m'
alias cp='cp -i'
alias zshreload="source ~/.zshrc"
alias ifconfig="ip -s -c -h a"
alias flux="systemctl --user start xfluxd"
42() { cd ~/Code/42/"$@" }
alias grep="grep --color=auto"
alias zshconfig="vim ~/.zshrc"
alias vimconfig="vim ~/.vimrc"

#GCC call
comp() { gcc -o "${1%.*}" "$1" -Wall -Wextra -Werror }
compc() { gcc -c -o "${1%.*}" "$1" -Wall -Wextra -Werror }

#Zathura shortcut
pdf() { zathura --fork "$1" }

# List directory contents
alias l='ls -la --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias ls='ls --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias ll='ls -l --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias la='ls -la --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'

# Push and pop directories on directory stack
alias pu='pushd'
alias po='popd'

# Mounting alias
ntfs() { sudo mount -t ntfs-3g /dev/"$@" /home/windows }
alias untfs='sudo umount /home/windows'
alias gontfs='cd ~/../windows'

# Git alias
alias upl='git add -all && git commit && git push'

# Keybinds
# Vi mode
bindkey -v

## Sources

BASE16_SHELL=$HOME/.config/base16-shell/
[ -n "$PS1" ] && [ -s $BASE16_SHELL/profile_helper.sh ] && eval "$($BASE16_SHELL/profile_helper.sh)"

# Powerline prompt
source ~/.shell_prompt.sh
# Fix colors
#source "$HOME/.vim/bundle/gruvbox/gruvbox_256palette.sh"
# Fixing keybindings
source "$HOME/.zsh/keybindings.zsh"
# Adding clipcopy and clippaste
source "$HOME/.zsh/clipboard.zsh"
# Directories shortcuts
source "$HOME/.zsh/directories.zsh"
# Syntax highlighting
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
