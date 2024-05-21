# Options, aliases and functions

if ps $$ | fgrep -q bash; then
  # Source Bash global definitions
  if [ -f /etc/bashrc ]; then
    . /etc/bashrc
  fi

  if [[ "$PS1" ]]; then
    # Bash completion; e.g. for Makefile targets
    if [ -f /usr/share/bash-completion/bash_completion ]; then
      . /usr/share/bash-completion/bash_completion
    elif [ -f /opt/local/share/bash-completion/bash_completion ]; then
      . /opt/local/share/bash-completion/bash_completion
    fi

    # For Emacs term-mode, especially when launched from a
    # daemon managed by systemd outside a login session.
    if [[ "$INSIDE_EMACS" && -f ~/.bash_profile ]]; then
      . ~/.bash_profile
    fi

    # Bash options
    if [[ "$BASH_VERSION" == 4.* ]]; then
      shopt -s dirspell
    fi
    shopt -s extglob
    shopt -s nocaseglob

    # Bash prompt
    if [[ "$TERM" == xterm* ]]; then
      PROMPT_COMMAND='printf "\033]0;%s@%s\033\\" "${USER}" "${HOSTNAME/.*/}"'
    fi
    if [[ -t 1 && "$(tput colors)" -gt 0 ]]; then
      PS1="\n\[\033[1;31m\]\t \[\033[0m\]\h:\[\033[1;34m\]\w\[\033[0m\]\n\$ "
    else
      PS1="\n\t \w\n\$ "
    fi
  fi
fi

umask 077
ulimit -c 0

if [ -d /Applications/Emacs.app/ ]; then
  alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'
  alias emacsclient='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient'
  alias etags='/Applications/Emacs.app/Contents/MacOS/bin/etags'
  alias ebrowse='/Applications/Emacs.app/Contents/MacOS/bin/ebrowse'
fi

alias pip='python3 -m pip'
alias pydoc='python3 -m pydoc'
alias python=python3

alias ls='ls -h'
alias rm='rm -i'

if [[ -t 1 && "$(tput colors)" -gt 0 ]]; then
  export LSCOLORS='ExGxcxdxCxegedabagacad'
  case "$OSTYPE" in
    linux*)
      alias ls='ls -h --color=auto'
      ;;
    darwin*)
      alias ls='ls -h -G'
      ;;
  esac
  alias grep='grep --color=auto'
  alias fgrep='fgrep --color=auto'
  alias egrep='egrep --color=auto'
fi

function cd {
  # cd with optional -l to follow resolve symlinks
  if [[ "$1" == '-l' ]]; then
    x="${2%/}"
    [[ -L "$x" ]] && builtin cd $(readlink "$x")
  else
    builtin cd "$@" > /dev/null
  fi
}
