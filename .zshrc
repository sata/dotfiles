export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="robbyrussell"
CASE_SENSITIVE="true"
plugins=(git ssh-agent)

export PATH="$PATH:$HOME/.rvm/bin"
export PATH="$PATH:/usr/local/bin:$HOME/.local/bin"
export PATH="$PATH:/usr/local/go/bin:$HOME/go/bin"
export PATH="$PATH:$HOME/sources/elixir-ls/release/"
export PATH="$HOME/.tfenv/bin:$PATH"

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
alias sås='cd ~/sources'
alias bctl='bluetoothctl'
alias k='kubectl'

extmonbrt () {
    ddcutil setvcp 10 $1 10 --display 1
}

. $HOME/.asdf/asdf.sh
. $HOME/.asdf/completions/asdf.bash

export GOPATH="$HOME/go"

export EDITOR="emacs -nw"

[[ -s "/home/s/.gvm/scripts/gvm" ]] && source "/home/s/.gvm/scripts/gvm"
if [ -e /home/s/.nix-profile/etc/profile.d/nix.sh ]; then . /home/s/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
