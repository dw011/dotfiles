if [ -f ~/.bashrc ]; then
  . ~/.bashrc
fi

unsetopt BEEP
setopt +o nomatch

bindkey -e
bindkey '\e[1;5C' forward-word
bindkey '\e[1;9C' forward-word
bindkey '\e[1;5D' backward-word
bindkey '\e[1;9D' backward-word

autoload -Uz compinit && compinit
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
