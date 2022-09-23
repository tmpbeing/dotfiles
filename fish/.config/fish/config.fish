set fish_greeting ""
set fish_home ~/.config/fish
set -x EDITOR nvim
set -x GIT_EDITOR nvim
set -x PAGER less
set -x MAIL42 mplanell@student.42.fr
set -x USER42 mplanell
set -x TASKRC $HOME/.config/taskrc
set -gx PATH $PATH $HOME/.krew/bin
set -gx PULUMI_SELF_MANAGED_STATE_GZIP 1

# Colored Man Pages
set -x MANPAGER "less -s -M +Gg"
set -g man_blink -o brmagenta
set -g man_bold -o brred
set -g man_standout -b brblack
set -g man_underline -u brgreen

# Handle ssh agent
source /home/snoop/.ssh/environment
fish_ssh_agent

# fzf.fish options
set fzf_fd_opts --hidden --exclude=.git
set fzf_preview_dir_cmd exa --all --color=always
