# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    .zshrc                                             :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: mplanell <mplanell@student.42.fr>          +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2016/08/14 13:32:43 by mplanell          #+#    #+#              #
#    Updated: 2017/02/04 16:52:49 by mplanell         ###   ########.fr        #
#                                                                              #
# **************************************************************************** #

# If not running interactively, do nothing
case $- in
	*i*) ;;
	  *) return ;;
esac

##### Options ######

setopt AUTO_CD #If invalid command is a directory name, cd to that directory
setopt CORRECT #Enables command correction
setopt NO_CLOBBER #Disables overwriting existing files with > (use >! instead)
setopt SHARE_HISTORY #All terminal sessions share the same history
setopt EXTENDED_HISTORY #Save timestamps as well
setopt HIST_IGNORE_DUPS #Do not enter immediate duplicates into history
setopt HIST_IGNORE_ALL_DUPS #Remove older instance if duplicate is added
setopt HIST_IGNORE_SPACE #History ignores commands starting with a space

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
export MANPAGER='less -s -M +Gg'

# Colors for man pages
export LESS_TERMCAP_mb=$'\E[1;31m'    # Begins blinking.
export LESS_TERMCAP_md=$'\E[1;31m'    # Begins bold.
export LESS_TERMCAP_me=$'\E[0m'       # Ends mode.
export LESS_TERMCAP_se=$'\E[0m'       # Ends standout-mode.
export LESS_TERMCAP_so=$'\E[7m'       # Begins standout-mode.
export LESS_TERMCAP_ue=$'\E[0m'       # Ends underline.
export LESS_TERMCAP_us=$'\E[1;32m'    # Begins underline.

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
alias weather="curl wttr.in/paris"
alias history-stat="history 0 | awk '{print \$2}' | sort | uniq -c | sort -n -r | head"
alias get='wget --continue --progress=bar --timestamping'
alias chmod='chmod --preserve-root -v'
alias chown='chown --preserve-root -v'
alias leaks='valgrind --leak-check=full'
alias gasm='gcc -Os -S'
alias cow='fortune | cowsay'

mkcd() {
  [[ -n ${1} ]] && mkdir -p ${1} && builtin cd ${1}
}

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
alias lm='l | ${PAGER}'
alias lr='l -R'
alias lrm='l -R | ${PAGER}'
alias lt='l -tr'

# Push and pop directories on directory stack
alias pu='pushd'
alias po='popd'

# Mounting alias
ntfs() { sudo mount -t ntfs-3g /dev/"$@" /home/windows }
alias untfs='sudo umount /home/windows'
alias gontfs='cd ~/../windows'

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
# Git Aliases
source "$HOME/.config/zsh/git.zsh"
# Syntax highlighting
source /usr/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
