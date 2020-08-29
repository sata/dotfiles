export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="robbyrussell"
CASE_SENSITIVE="true"
plugins=(git ssh-agent)

export PATH="$PATH:$HOME/.rvm/bin"
export PATH="$PATH:/usr/local/bin:$HOME/.local/bin"
export PATH="$PATH:/usr/local/go/bin:$HOME/go/bin"
export PATH="$PATH:$HOME/sources/elixir-ls/release/"

source $ZSH/oh-my-zsh.sh

zstyle :omz:plugins:ssh-agent agent-forwarding on
zstyle :omz:plugins:ssh-agent identities id_rsa fatcat.id_rsa fatcat.initramsfs.id_rsa mbt.id_rsa peupeu.id_rsa rpi.id_rsa wintermute.id_rsa

setopt NO_HUP

HISTSIZE=100000
SAVESIZE=100000

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

alias dh="ssh first_one"
alias rpi="ssh rpi"

alias gr="grep -ri"
alias ll='ls -alh'
alias l='ls -lh'

. $HOME/.asdf/asdf.sh
. $HOME/.asdf/completions/asdf.bash

export GO111MODULE=auto
export GOPATH="$HOME/go"

export EDITOR="emacs -nw"
