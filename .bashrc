# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

export OS_NAME=$(uname -s)

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

# don't put duplicate lines or lines starting with space in the history.
# also remove from history any previous duplicate.
# See bash(1) for more options
HISTCONTROL=ignoreboth:erasedups

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=10000
HISTFILESIZE=512000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
#force_color_prompt=yes

if [ -n "$force_color_prompt" ]; then
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
	# We have color support; assume it's compliant with Ecma-48
	# (ISO/IEC-6429). (Lack of such support is extremely rare, and such
	# a case would tend to support setf rather than setaf.)
	color_prompt=yes
    else
	color_prompt=
    fi
fi

if [ "$color_prompt" = yes ]; then
    PS1='${debian_chroot:+($debian_chroot)}\[\033[01;32m\]\u@\h\[\033[00m\]:\[\033[01;34m\]\w\[\033[00m\]\$ '
else
    PS1='${debian_chroot:+($debian_chroot)}\u@\h:\w\$ '
fi
unset color_prompt force_color_prompt

# If this is an xterm set the title to user@host:dir
case "$TERM" in
xterm*|rxvt*)
    PS1="\[\e]0;${debian_chroot:+($debian_chroot)}\u@\h: \w\a\]$PS1"
    ;;
*)
    ;;
esac

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
#export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

# some more ls aliases
alias ll='ls -alFh'
alias la='ls -A'
alias l='ls -CF'

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

function add_dir_to_path_if_missing {
    local dir="${1}"
    local postfix="$( [ "${2}" = "postfix" ] && echo "postfix" )"
    local pattern="${dir}:"

    if [ -n "${postfix}" ]; then
        pattern=":${dir}"
    fi

    if [ -z "$(echo "${PATH}" | grep -o "${pattern}")" ] && [ -d "${dir}" ]; then
        if [ -n "${postfix}" ]; then
            export PATH="${PATH}${pattern}"
        else
            export PATH="${pattern}${PATH}"
        fi
    fi
}

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# share history among all terminals
PROMPT_COMMAND="${PROMPT_COMMAND:+${PROMPT_COMMAND}$'\n'}history -a; history -c; history -r"

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi


#
#  Homebrew support
#

BREW="$(which brew)"
if [ -n "${BREW}" ]; then
    BREW_PATH="$(${BREW} --prefix)"
fi


#
#  Alacritty Autocompletion
#

ALACRITTY="$(which alacritty || which /Applications/Alacritty.app/Contents/MacOS/alacritty)"
if [ -n "${ALACRITTY}" ] && [ -f "${HOME}/.config/alacritty/alacritty.bash" ]; then
    . "${HOME}/.config/alacritty/alacritty.bash"
fi


#
#  Setup Python3 AND Powerline
#

export PYTHON_VERSION="$($(which python3) --version | awk '{print $2}' | egrep -o '^[0-9]\.[0-9][0-9]*')"

export PY_DIR="${HOME}/.local/lib/python${PYTHON_VERSION}"
if [ "${OS_NAME}" = "Darwin" ]; then
    export PY_DIR="${HOME}/Library/Python/${PYTHON_VERSION}"
fi
export POWERLINE_PY_PKG_DIR="${PY_DIR}/site-packages"
export POWERLINE_BIN_DIR="${HOME}/.local/bin"
if [ "${OS_NAME}" = "Darwin" ]; then
    export POWERLINE_PY_PKG_DIR="${PY_DIR}/lib/python/site-packages"
    export POWERLINE_BIN_DIR="${PY_DIR}/bin"
fi


#
#  pyenv support
#

PYENV="$(which pyenv)"
if [ -n "${PYENV}" ]; then
    export PYENV_ROOT="${HOME}/.pyenv"
    add_dir_to_path_if_missing "${PYENV_ROOT}"
fi


add_dir_to_path_if_missing "${HOME}/bin"
add_dir_to_path_if_missing "${HOME}/.local/bin"


#
#  VScode support
#

if [ "${OS_NAME}" = "Darwin" ]; then
    add_dir_to_path_if_missing "/Applications/Visual Studio Code.app/Contents/Resources/app/bin" "postfix"
fi


#
#  Powerline support
#

if [ -z "$(echo "${PATH}" | grep -o "${POWERLINE_BIN_DIR}:")" ] && [ -d "${POWERLINE_BIN_DIR}" ]; then
    export PATH="${POWERLINE_BIN_DIR}:$PATH"
fi
if [ -f "${POWERLINE_PY_PKG_DIR}/powerline/bindings/bash/powerline.sh" ]; then
    ${POWERLINE_BIN_DIR}/powerline-daemon -q
    export POWERLINE_BASH_CONTINUATION=1
    export POWERLINE_BASH_SELECT=1
    . "${POWERLINE_PY_PKG_DIR}/powerline/bindings/bash/powerline.sh"
fi


#
#  Setup Python Virtual Environment suooprt
#

export WORKON_HOME=${HOME}/.virtualenvs
export PROJECT_HOME=${HOME}/wrk
VIRTUAL_ENV_WRAPPER_SCRIPT="$(which virtualenvwrapper.sh)"
if [ -n "${VIRTUAL_ENV_WRAPPER_SCRIPT}" ]; then
    . ${VIRTUAL_ENV_WRAPPER_SCRIPT}
fi


#
#  Setup NVM
#

export NVM_DIR="${HOME}/.nvm"
[ -s "${NVM_DIR}/nvm.sh" ] && \. "${NVM_DIR}/nvm.sh"                    # This loads nvm
[ -s "${NVM_DIR}/bash_completion" ] && \. "${NVM_DIR}/bash_completion"  # This loads nvm bash_completion


#
#  Setup Rust
#

if [ -f "${HOME}/.cargo/env" ]; then
    . "${HOME}/.cargo/env"
fi


#
#  Setup Go
#

GO="$(which go)"
if [ -z "${GO}" ] && [ -n "${BREW}" ]; then
    for G in "${BREW_PATH}/opt/go"*; do
        add_dir_to_path_if_missing "${G}/bin"
    done
    GO="$(which go)"
fi
if [ -n "${GO}" ]; then
    if [ ! -f "${HOME}/.golang" ]; then
        mkdir -p "${HOME}/.golang"
    fi
    if [ -z "${GOPATH}" ]; then
        export GOPATH="${HOME}/.golang"
    else
        if [ -z "$(echo "${GOPATH}" | grep -o "${HOME}/.golang:")" ] && [ -d "${HOME}/.golang" ]; then
            export GOPATH="${HOME}/.golang:${GOPATH}"
        fi
    fi
    # if [ -d "${HOME}/wrk" ]; then
    #     export GOPATH="${GOPATH}:${HOME}/wrk"
    # fi
    add_dir_to_path_if_missing "${GOPATH}/bin"
fi


#
#  MinIO Client Autocompletion
#

MINIO_CLIENT="$(which mc)"
if [ -n "${MINIO_CLIENT}" ] && [ "$(${MINIO_CLIENT} --help | grep -o 'MinIO Client')" == "MinIO Client" ]; then
    complete -C ${MINIO_CLIENT} mc
fi


#
#  Setup kubectl
#

KUBECTL="$(which kubectl)"
if [ -n "${KUBECTL}" ] && [ "$(${KUBECTL} version --client | grep -o 'Client Version: version.Info')" == "Client Version: version.Info" ]; then
    . <(${KUBECTL} completion bash)
    if [ -d "${HOME}/.kube" ]; then
        for _CONF_ in $(ls "${HOME}/.kube/"); do
            if [ -z "$(echo "${KUBECONFIG}" | grep -o ":${HOME}/.kube/${_CONF_}")" ] && [ -f "${HOME}/.kube/${_CONF_}" ]; then
                export KUBECONFIG="${KUBECONFIG}:${HOME}/.kube/${_CONF_}"
            fi
        done
        export KUBECONFIG=$(echo "${KUBECONFIG}" | sed -e "s/^://" -e "s/:$//")
    fi
fi


#
#  Setup fzf
#

FZF="$(which fzf)"
if [ -n "${FZF}" ]; then
    if [ -n "${BREW}" ] && [ -d "${BREW_PATH}/Cellar/fzf/"**"/shell/" ]; then
        . "${BREW_PATH}/Cellar/fzf/"**"/shell/key-bindings.bash"
        . "${BREW_PATH}/Cellar/fzf/"**"/shell/completion.bash"
    fi
fi
