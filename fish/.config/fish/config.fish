set fish_greeting ""
set fish_home ~/.config/fish
set -x EDITOR nvim
set -x GIT_EDITOR nvim
set -x PAGER less
set -x MAIL42 mplanell@student.42.fr
set -x USER42 mplanell
set -x TASKRC $HOME/.config/taskrc

# Colored Man Pages
set -x MANPAGER "less -s -M +Gg"
set -g man_blink -o brmagenta
set -g man_bold -o brred
set -g man_standout -b brblack
set -g man_underline -u brgreen

# Handle ssh agent
source /home/snoop/.ssh/environment
fish_ssh_agent

# THEME PURE #
set fish_function_path /home/snoop/.config/fish/functions/theme-pure $fish_function_path
# THEME PURE #
set fish_function_path /home/snoop/.config/fish/functions/theme-pure/functions/ $fish_function_path
source /home/snoop/.config/fish/functions/theme-pure/conf.d/pure.fish
# THEME PURE #
set fish_function_path /home/snoop/.config/fish/functions/theme-pure/functions/ $fish_function_path
source /home/snoop/.config/fish/functions/theme-pure/conf.d/pure.fish

set pure_color_symbol_success $pure_color_green
set pure_color_current_folder $pure_color_magenta
set pure_color_git_branch $pure_color_blue
