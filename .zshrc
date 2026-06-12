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

# Pure prompt: https://github.com/sindresorhus/pure
fpath+=($HOME/.zsh/pure)
autoload -U promptinit && promptinit
prompt pure


# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/daniel/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/daniel/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/daniel/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/daniel/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<
