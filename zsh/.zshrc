# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    .zshrc                                             :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: mplanell <mplanell@student.42.fr>          +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2016/08/14 13:32:43 by mplanell          #+#    #+#              #
#    Updated: 2017/06/28 17:03:00 by mplanell         ###   ########.fr        #
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
setopt PUSHD_IGNORE_DUPS #Do not push duplicates into the directory stack
setopt AUTO_PUSHD #Automatically add pwd to directory stack
# Vi mode
bindkey -v

##### Exports ######

# 42 Header variable
export MAIL42=mplanell@student.42.fr
export USER42=mplanell

# Other
# export TZ=Europe/Paris
export EDITOR="vim"
export PAGER="less"
export MANPAGER='less -s -M +Gg'
export REPORTTIME=10 # Display how long all tasks over 10 seconds take

# Colors for man pages
# export LESS_TERMCAP_mb=$'\E[1;31m'    # Begins blinking.
# export LESS_TERMCAP_md=$'\E[1;31m'    # Begins bold.
# export LESS_TERMCAP_me=$'\E[0m'       # Ends mode.
# export LESS_TERMCAP_se=$'\E[0m'       # Ends standout-mode.
# export LESS_TERMCAP_so=$'\E[7m'       # Begins standout-mode.
# export LESS_TERMCAP_ue=$'\E[0m'       # Ends underline.
# export LESS_TERMCAP_us=$'\E[1;32m'    # Begins underline.
export LESS_TERMCAP_mb=$'\e[0;31m'
export LESS_TERMCAP_md=$'\e[0;34m'
export LESS_TERMCAP_me=$'\e[0m'
export LESS_TERMCAP_se=$'\e[0m'
export LESS_TERMCAP_so=$'\e[0;34;36m'
export LESS_TERMCAP_ue=$'\e[0m'
export LESS_TERMCAP_us=$'\e[0;35m'

##### Aliases ######

# Convenience
alias vim='nvim'
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
alias vimch='vim *[.c/.h]'
alias gdb='gdbgui'
alias 42fc='sh ~/Code/42/42FileChecker/42FileChecker.sh'
alias fixscreen='xrandr --output DVI-1 --auto'

mkcd() {
  [[ -n ${1} ]] && mkdir -p ${1} && builtin cd ${1}
}

conf() {
	case $1 in
		i3)			vim ~/.config/i3/config ;;
		zsh)			vim ~/.zshrc && source ~/.zshrc ;;
		vim)			vim ~/.vimrc ;;
		nvim)			vim ~/.config/nvim/init.vim ;;
		xinit)			vim ~/.xinitrc ;;
		xresources)		vim ~/.Xresources ;;
		ranger)			vim ~/.config/ranger/rc.conf ;;
		pacman)			sudo vim /etc/pacman.conf ;;
		zathura)		vim ~/.config/zathura/zathurarc ;;
		dunst)			vim ~/.config/dunst/dunstrc ;;
		compton)		vim ~/.config/compton/compton.conf ;;
		termite)		vim ~/.config/termite/config ;;
		i3blocks)		vim ~/.config/i3blocks/config ;;
		rofi)			vim ~/.config/rofi/config ;;
		polybar)		vim ~/.config/polybar/config ;;
		*)			echo "Unknown application $1" ;;
	esac
}

#GCC call
comp() { gcc -o "${1%.*}" "$1" -Wall -Wextra -Werror }
compc() { gcc -c -o "${1%.*}" "$1" -Wall -Wextra -Werror }

#Zathura shortcut
pdf() { zathura --fork "$1" }

# List directory contents
alias l='ls -la'
alias ls='ls --group-directories-first --time-style=+"%d.%m.%Y %H:%M" --color=auto -F'
alias ll='ls -l'
alias la='ls -la'
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

##### Prompt #####

POWERLEVEL9K_LEFT_SEGMENT_SEPARATOR="▓▒░"
POWERLEVEL9K_RIGHT_SEGMENT_SEPARATOR="░▒▓"
POWERLEVEL9K_LEFT_SUBSEGMENT_SEPARATOR="▓▒░"
POWERLEVEL9K_RIGHT_SUBSEGMENT_SEPARATOR="░▒▓"
POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=(dir vcs)
POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=(status virtualenv dir_writable)
POWERLEVEL9K_STATUS_VERBOSE=true
POWERLEVEL9K_STATUS_OK_IN_NON_VERBOSE=false
POWERLEVEL9K_SHORTEN_DIR_LENGTH=3
POWERLEVEL9K_COMMAND_EXECUTION_TIME_THRESHOLD=5
POWERLEVEL9K_MODE='awesome-fontconfig'
POWERLEVEL9K_CONTEXT_TEMPLATE="%n"
POWERLEVEL9K_VCS_GIT_BITBUCKET_ICON=''
POWERLEVEL9K_VCS_GIT_GITHUB_ICON=''
POWERLEVEL9K_VCS_GIT_GITLAB_ICON=''
POWERLEVEL9K_VCS_GIT_ICON=''
POWERLEVEL9K_HOME_ICON=''
POWERLEVEL9K_HOME_SUB_ICON=''
POWERLEVEL9K_FOLDER_ICON=''
POWERLEVEL9K_LOCK_ICON=''
POWERLEVEL9K_DIR_WRITABLE_FORBIDDEN_BACKGROUND='5'
POWERLEVEL9K_DIR_WRITABLE_FORBIDDEN_FOREGROUND='0'
POWERLEVEL9K_STATUS_ERROR_BACKGROUND='11'
POWERLEVEL9K_STATUS_ERROR_FOREGROUND='0'
POWERLEVEL9K_OK_ICON=''
POWERLEVEL9K_DIR_HOME_FOREGROUND="%b0"
POWERLEVEL9K_DIR_HOME_SUBFOLDER_FOREGROUND="%b0"
POWERLEVEL9K_DIR_DEFAULT_FOREGROUND="%b0"

##### Sources #####

# Fix Colors
source "$HOME/.vim/bundle/gruvbox/gruvbox_256palette.sh"
# Prompt
source ~/.config/powerlevel9k/powerlevel9k.zsh-theme
#source "$HOME/.config/.shell_prompt.sh"
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

cow
