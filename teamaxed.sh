#!/usr/bin/env bash

### Used by Alacritty to start tmux (on top of bash!) as "shell" to provide additional features seamlessly

if which tmux &> /dev/null && [ "${SHELL}" == "/bin/bash" ] && [[ ! "${TERM}" =~ screen ]] && [[ ! "${TERM}" =~ tmux ]] && [ -z "${TMUX}" ]; then
  tmux a -t $(whoami) || tmux new -s $(whoami)
fi
