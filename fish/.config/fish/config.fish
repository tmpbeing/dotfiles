# Icons-in-terminal
source ~/.local/share/icons-in-terminal/icons.fish

set fish_greeting ""
set fish_home ~/.config/fish
set PAGER less
set EDITOR nvim
set GIT_EDITOR nvim
# Colored Man Pages
set -x MANPAGER "less -m -g +Gg"
set -g man_blink -o brmagenta
set -g man_bold -o brred
set -g man_standout -b brblack
set -g man_underline -u brgreen

source $fish_home/environment.fish
# THEME PURE #
set fish_function_path /home/snoop/.config/fish/functions/theme-pure $fish_function_path
