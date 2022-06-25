if [ -f ~/.bashrc ]; then
  . ~/.bashrc
fi

unsetopt BEEP
setopt +o nomatch

bindkey -e
bindkey '\e[1;5C' end-of-line
bindkey '\e\e[C' forward-word
bindkey '\e[1;5D' beginning-of-line
bindkey '\e\e[D' backward-word

autoload -Uz compinit && compinit
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Za-z}'
