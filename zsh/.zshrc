# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    .zshrc                                             :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: mplanell <mplanell@student.42.fr>          +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2016/08/14 13:32:43 by mplanell          #+#    #+#              #
#    Updated: 2017/01/24 17:38:06 by mplanell         ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

# If not running interactively, do nothing
case $- in
	*i*) ;;
	  *) return ;;
esac

##### Options ######

setopt AUTO_CD
setopt CORRECT

# Keybinds
# Vi mode
bindkey -v

##### Exports ######

# 42 Header variable
export MAIL42=mplanell@student.42.fr
export USER42=mplanell

# Other
export EDITOR="vim"
export PAGER="less"

# Colors for man pages
export LESS_TERMCAP_mb=$'\e[0;36m'
export LESS_TERMCAP_md=$'\e[0;31m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[0;34;32m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[0;34m'

##### Aliases ######

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
alias gcc42="gcc -Wall -Wextra -Werror"
alias 3d="toilet -t -f 3d"
alias gay3d="toilet -t -f 3d -F gay"

conf() {
	case $1 in
		i3)				vim ~/.config/i3/config ;;
		zsh)			vim ~/.zshrc && source ~/.zshrc ;;
		vim)			vim ~/.vimrc ;;
		xinit)			vim ~/.xinit ;;
		xresources)		vim ~/.Xresources ;;
		ranger)			vim ~/.config/ranger/rc.conf ;;
		pacman)			sudo vim /etc/pacman.conf ;;
		zathura)		vim ~/.config/zathura/zathurarc ;;
		dunst)			vim ~/.config/dunst/dunstrc ;;
		comptom)		vim ~/.config/compton/compton.conf ;;
		termite)		vim ~/.config/termite/config ;;
		i3blocks)		vim ~/.config/i3blocks/config ;;
		rofi)			vim ~/.config/rofi/config ;;
		*)			echo "Unknown application $1" ;;
	esac
}

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

##### Sources #####

# Fix Colors
source "$HOME/.vim/bundle/gruvbox/gruvbox_256palette.sh"
# Prompt
source "$HOME/.config/.shell_prompt.sh"
# Fixing keybindings
source "$HOME/.config/zsh/keybindings.zsh"
# Adding clipcopy and clippaste
source "$HOME/.config/zsh/clipboard.zsh"
# Directories shortcuts
source "$HOME/.config/zsh/directories.zsh"
# Syntax highlighting
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
