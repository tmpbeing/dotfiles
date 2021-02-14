[ -d "$ZGEN_DIR" ] || git clone https://github.com/tarjoilija/zgen "$ZGEN_DIR"
source $ZGEN_SOURCE
if ! zgen saved; then
  echo "Initializing zgen"
  zgen load hlissner/zsh-autopair autopair.zsh
  zgen load zsh-users/zsh-history-substring-search
  zgen load zdharma/history-search-multi-word
  zgen load zsh-users/zsh-completions src
  zgen load junegunn/fzf shell
  [ -z "$SSH_CONNECTION" ] && zgen load zdharma/fast-syntax-highlighting
  zgen load denysdovhan/spaceship-prompt spaceship
  zgen save
fi

source $ZDOTDIR/aliases.zsh
source $ZDOTDIR/completions.zsh

export MONITOR=$(xrandr -q | grep primary | cut -d' ' -f1)
# export MONITORS=( $(xrandr -q | grep ' connected' | cut -d' ' -f1) )
