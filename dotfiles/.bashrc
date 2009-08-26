export R_LIBS=$HOME/R
alias R='R --no-save --no-restore-data --quiet'

export INPUTRC=~/.inputrc 
export EDITOR="gedit" 

# Save lots of history 
export HISTFILESIZE=10000 
export HISTSIZE=10000 
export HISTCONTROL=erasedups 
shopt -s histappend 

# Show which directory you're working in 
export PS1="\W: "