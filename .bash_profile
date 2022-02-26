# Environment initialization

if ps $$ | fgrep -q bash; then
  # Include user options, aliases and functions in login shells
  if [ -f ~/.bashrc ]; then
    . ~/.bashrc
  fi

  # Bash completion; e.g. for Makefile targets
  if [ -f /opt/local/etc/profile.d/bash_completion.sh ]; then
    . /opt/local/etc/profile.d/bash_completion.sh
  fi
fi

if [ -f /Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs ]; then
  export EDITOR='/Applications/MacPorts/Emacs.app/Contents/MacOS/Emacs -nw';
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
  export JAVA_HOME=$(/usr/libexec/java_home)
  export KEYTOOL="$JAVA_HOME"/jre/bin
fi
