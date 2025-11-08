# Lines configured by zsh-newuser-install
HISTFILE=~/.histfile
HISTSIZE=1000
SAVEHIST=1000
bindkey -e
zstyle :compinstall filename '/home/parth/.zshrc'
eval "$(starship init zsh)"
autoload -Uz compinit
compinit

source ~/.config/zsh-autosuggestions.zsh

alias sudo="sudo "
alias ls="eza -l --group-directories-first"
alias ll="ls -al"
alias vi="nvim"
alias update="yay -Syyu"
alias grep="grep --color=always -i"
alias cat="bat --color=always"
alias fzf="fzf --preview='bat {}'"
