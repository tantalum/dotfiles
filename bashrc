# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Start ssh-agent
if [ -z "$SSH_AUTH_SOCK" ]; then
    eval "$(ssh-agent -s)" > /dev/null
    #ssh-add ~/.ssh/id_rsa 2>/dev/null
fi

colors='false'

case $TERM in 
    *color*)
        colors='true';;
esac

cyan=$(tput setaf 6)
pink=$(tput setaf 5)
blue=$(tput setaf 4)
reset=$(tput sgr0)

# Set color prompts
if [[ $colors -eq "true" ]]; then
    export PS1="[\[$cyan\]\\u@\\h\[$reset\] \[$pink\]\\W\[$reset\]]\\$ "
else
    export PS1="[\u@\h \W]\\$ "
fi

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

# Git Prompt
if [[ -f /usr/share/git-core/contrib/completion/git-prompt.sh ]]
then
    source /usr/share/git-core/contrib/completion/git-prompt.sh
    if [[ $colors -eq "true" ]]; then
        export PS1="[\[$cyan\]\\u@\\h\[$reset\] \[$pink\]\\W\[$reset\]] \[$blue\]\$(__git_ps1)\[$reset\]\\$ "
    else
        export PS1="[\u@\h \W] \\$(__git_ps1)\\$ "
    fi
fi

# Ruby Version Manager
if [[ -f ~/.rvm/scripts/rvm ]]
then
    source ~/.rvm/scripts/rvm
fi

# Node Version Manager
export NVM_DIR="$HOME/.config"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
