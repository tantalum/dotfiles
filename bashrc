# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

colors='false'

case $TERM in 
    *color*)
        colors='true';;
esac

# Set color prompts
#if [[ $colors -eq "true" ]]; then
#    export PS1="[\e[0;36m\\u@\\h \e[0;35m\\W\e[m]\\$ "
#else
#    export PS1="[\\u@\\h \\W]\\$ "
#fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]
then
    PATH="$HOME/.local/bin:$HOME/bin:$HOMEAndroid/Sdk/platform-tools:$PATH"
fi
export PATH

# Define our aliases
if [[ -f ~/.alias ]]
then
    source ~/.alias
fi

# Custom environment variables
if [[ -f ~/.env ]]
then
    source ~/.env
fi

EDITOR="vim"
