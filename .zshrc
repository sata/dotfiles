export ZSH=$HOME/.oh-my-zsh

ZSH_THEME="robbyrussell"
CASE_SENSITIVE="true"
plugins=(git ssh-agent zsh-autosuggestions)

source $ZSH/oh-my-zsh.sh

unalias gau

export PATH="$PATH:/usr/local/bin:$HOME/.local/bin"
export PATH="$PATH:/usr/local/go/bin:$HOME/go/bin"
export PATH="$PATH:$HOME/sources/elixir-ls/release/"
export PATH="$PATH:$HOME/.cargo/bin"
export PATH="$PATH:/home/s/.cache/rebar3/bin"

setopt NO_HUP

HISTSIZE=100000
SAVESIZE=100000

alias dh="ssh first_one"
alias rpi="ssh rpi"

alias gr="grep -ri"
alias ll='ls -alh'
alias l='ls -lh'
alias sås='cd ~/sources'
alias bctl='bluetoothctl'
alias k='kubectl'
alias t='i3-sensible-terminal&'
alias dotfiles='cd ~/sources/dotfiles'
alias books='cd ~/sources/books'

extmonbrt () {
    ddcutil setvcp 10 $1 20 --display 1
}

lsh() {
    dir="$1"
    if [ -z "$dir" ]
    then
        dir="."
    fi

    ls -ltc --color=always "$dir" | head -n 20
}

. $HOME/.asdf/asdf.sh

export GOPATH="$HOME/go"

export EDITOR="emacsclient -nw"

source ~/.banner.sh

if [ -e /home/s/.nix-profile/etc/profile.d/nix.sh ]; then . /home/s/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer
