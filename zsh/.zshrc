# **************************************************************************** #
#                                                                              #
#                                                         :::      ::::::::    #
#    .zshrc                                             :+:      :+:    :+:    #
#                                                     +:+ +:+         +:+      #
#    By: mplanell <mplanell@student.42.fr>          +#+  +:+       +#+         #
#                                                 +#+#+#+#+#+   +#+            #
#    Created: 2016/08/14 13:32:43 by mplanell          #+#    #+#              #
#    Updated: 2017/03/08 10:15:29 by mplanell         ###   ########.fr        #
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
MAIL="mplanell@student.42.fr"
export MAIL

# Other
export EDITOR="nvim"

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

# Colors for man pages
export LESS_TERMCAP_mb=$'\E[1;31m'    # Begins blinking.
export LESS_TERMCAP_md=$'\E[1;31m'    # Begins bold.
export LESS_TERMCAP_me=$'\E[0m'       # Ends mode.
export LESS_TERMCAP_se=$'\E[0m'       # Ends standout-mode.
export LESS_TERMCAP_so=$'\E[7m'       # Begins standout-mode.
export LESS_TERMCAP_ue=$'\E[0m'       # Ends underline.
export LESS_TERMCAP_us=$'\E[1;32m'    # Begins underline.

## Aliases

# Convenience
alias vim='nvim'
alias c='clear'
alias h='history'
alias update='pacaur -Syu'
alias free='free -m'
alias cp='cp -i'
alias zshreload="source ~/.zshrc"
alias ifconfig="ip -s -c -h a"
alias grep="grep --color=auto"
alias gcc42='gcc -Wall -Werror -Wextra'
alias weather='curl wttr.in/paris'
alias gasm='gcc -Os -S'
alias vimch='vim *[.c/.h]'
alias norm='norminette *[.c/.h]'
alias 42fc='sh ~/projects/42FileChecker/42FileChecker.sh'

mkcd() {
  [[ -n ${1} ]] && mkdir -p ${1} && builtin cd ${1}
}

conf() {
	case $1 in
		vim)		vim ~/.vimrc ;;
		zsh)		vim ~/.zshrc && source ~/.zshrc ;;
		nvim)		vim ~/.config/nvim/init.vim ;;
		*)			echo "Unknown application $1" ;;
	esac
}

gitnorm() {
	norminette $(git ls-files)
}

#GCC call
comp() { gcc -o "${1%.*}" "$1" -Wall -Wextra -Werror }
compc() { gcc -c -o "${1%.*}" "$1" -Wall -Wextra -Werror }

# List directory contents
alias l='ls -lah -FG'
alias ls='ls -FG'
alias ll='ls -l -FG'
alias la='ls -la -FG'
alias lm='l | ${PAGER}'
alias lr='l -R'
alias lt='l -tr'

# Push and pop directories on directory stack
alias pu='pushd'
alias po='popd'

# Git alias
alias upl='git add -all && git commit && git push'

# Keybinds
# Vi mode
bindkey -v

# Powerline prompt
source ~/.shell_prompt.sh
source ~/dotfiles/zsh/.gruvbox_256palette_osx.sh
source ~/dotfiles/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Load Homebrew config script
source $HOME/.brewconfig.zsh
