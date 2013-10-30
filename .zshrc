# --------------------------------------------------------------------
# BEGIN: Default Configuration
# #-------------------------------------------------------------------

# Set up the prompt

autoload -Uz promptinit
promptinit
prompt adam2

setopt histignorealldups sharehistory

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

# Keep 500 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=500
SAVEHIST=500
HISTFILE=~/.zsh_history

# Use modern completion system
autoload -Uz compinit
compinit

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

#--------------------------------------------------------------------------
#END: Default Configuration
#--------------------------------------------------------------------------

# Command Aliases
source $HOME/.alias

# Environment Variables
export PATH="/sbin:$HOME/bin:$HOME/.rvm/bin:$PATH"
export PKG_CONFIG_PATH="$PKG_CONFIG_PATH:$HOME/lib/pkgconfig"
export LD_LIBRARY_PATH="$HOME/lib"
export HOSTNAME=`hostname`

# Set screen title
case "$TERM" in
	screen*) precmd() { 
		wd=`pwd`
		wd=`basename $wd`
		echo -ne "\033k$wd@$HOSTNAME\033\\" 
	} ;;
esac

# Load RVM into a shell session *as a function*
[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" > $HOME/.rvm/start.log
rvm use 1.9.3 > $HOME/.rvm/start.log

# DevilsPie
if pgrep gnome-session > /dev/null; then
    if ! pgrep devilspie >/dev/null; then 
        devilspie -a > $HOME/.devilspie/debug.log & # Run devils pie window hacker 
    fi
fi
