# Environment initialization

if ps $$ | fgrep -q bash; then
  # Include user options, aliases and functions in login shells
  if [ -f ~/.bashrc ]; then
    . ~/.bashrc
  fi
fi

if [ -f /Applications/Emacs.app/Contents/MacOS/Emacs ]; then
  export EDITOR='/Applications/Emacs.app/Contents/MacOS/Emacs -nw';
else
  export EDITOR='emacs -nw';
fi
export MANPATH=/opt/local/share/man:"$MANPATH":~/.local/share/man
export PAGER=/usr/bin/less
export PATH=/opt/local/bin:/opt/local/sbin:"$PATH":~/.local/bin

# Unicode setup
export LANG=en_US.UTF-8
export PERL_UNICODE=SDA
export PYTHONIOENCODING=UTF-8

# Java environment
if [ -x /usr/libexec/java_home ]; then
  export JAVA_HOME=$(/usr/libexec/java_home 2> /dev/null)
  if [[ "$JAVA_HOME" ]]; then
    export KEYTOOL="$JAVA_HOME"/jre/bin
  else
    unset JAVA_HOME
  fi
fi
