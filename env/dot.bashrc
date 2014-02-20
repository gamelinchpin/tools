# .bashrc
#
# Add your aliases, envvars, and functions to this file.
#
# You should leave everything in this file up to the "User-defined" section
# alone (unless you know what you're doing).


# Determine where the global interactive *rc is located.
BASHRC_INTERACTIVE=''
if [ -f ~/.bashrc.interactive ]; then
    BASHRC_INTERACTIVE=~/.bashrc.interactive
elif [ -f ~/bashrc.interactive ]; then
    BASHRC_INTERACTIVE=~/bashrc.interactive
elif [ -f /etc/bashrc.interactive ]; then
    BASHRC_INTERACTIVE=/etc/bashrc.interactive
fi

# Get the aliases and functions used in interactive shells.
# Global (but only if it hasn't been sourced yet)
if [ -n "$PS1" -a -z "${INTERACTIVE}" ]; then
    if [ -x /etc/profile.d/000_bashrc_local.sh ]; then
        # Sets the $INTERACTIVE var and runs the global bashrc.interactive.
        . /etc/profile.d/000_bashrc_local.sh
    else
        # Do it ourselves.
        case $- in
            *i*)
                export INTERACTIVE="yes"
                ;;
        esac
    fi
fi


# Punt if non-interactive.
if [ -z "${INTERACTIVE}" ]; then
    unset BASHRC_INTERACTIVE
    return
fi


#############################################################################
#
# Debian/(K)Ubuntu defaults.
#
#############################################################################


# don't put duplicate lines in the history. See bash(1) for more options
# don't overwrite GNU Midnight Commander's setting of `ignorespace'.
export HISTCONTROL=$HISTCONTROL${HISTCONTROL+,}ignoredups
# ... or force ignoredups and ignorespace
export HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set variable identifying the chroot you work in (used in the prompt below)
if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
    debian_chroot=$(cat /etc/debian_chroot)
fi


#### [jpw]
# Conditional to disable the defaults when we're gonna source
# $BASHRC_INTERACTIVE
if [ -z "${BASHRC_INTERACTIVE}" ]; then
    echo "Loading Debian/Ubuntu bash-defaults."

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color) color_prompt=yes;;
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

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    eval "`dircolors -b`"
    alias ls='ls --color=auto'
    alias dir='dir --color=auto'
    alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# some more ls aliases
alias ll='ls -l'
alias la='ls -A'
alias l='ls -CF'

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi


#### [jpw]
fi # [ -z $BASHRC_INTERACTIVE ]


########################################
#
# Source the global interactive file.
#
########################################


if [ -n "$INTERACTIVE" -a -n "$BASHRC_INTERACTIVE" ]; then
    . $BASHRC_INTERACTIVE
fi


#############################################################################
#
# User-Defined Aliases, Variables, Functions
#
#############################################################################


STTYPE="${MACHTYPE}-${OSTYPE}"


alias veryclean="(cd ; clean.sh -r)"

alias repath="export PATH=${PATH}"
alias resharc='source ~/.bashrc'
alias resharc_all='INTERACTIVE="y" source ~/.bashrc'


if [ -n "$TAR_AUTOCOMPRESS_OPT" ]; then
    alias untar='tar -axvf'
fi
alias untarz='tar -zxvf'
alias untarb='tar -jxvf'
alias uncpioz='uncpio --gzip'
alias uncpiob='uncpio --bzip2'

r_enscrCmd="enscript -v -D Duplex:true --non-printable-format=caret"
alias twoup="$r_enscrCmd -2 -j -f Courier-Bold7 --landscape"
alias enscript_lp="$r_enscrCmd -1 -l"
unset r_enscrCmd

alias odx='od -A x -t x1z'

# CVS Stuff
#
##alias cvsmerge='cvs update -kk -j'
##alias cvsmerge-head='cvs update -kk -j HEAD'
case ${OSTYPE} in
    [Cc]ygwin|*linux*)
        export PATH="${PATH}:${HOME}/bin"
        if [ -e ${HOME}/local/bin ]; then
            export PATH="${PATH}:${HOME}/local/bin"
        fi
        alias repath="export PATH=${PATH}"
        ;;
esac


#######################################################################
#
# Non-Cygwin Aliases/Vars below this line.
#
#######################################################################


export NAME="John Weiss"
export PRINTER="ps"
#export CVSROOT=${HOME}/src/0_Local_CVS_Repos
#export BIBINPUTS="${HOME}/docs/refs:"
#export BSTINPUTS="${BIBINPUTS}:"
#export TEXINPUTS=".//:${HOME}/docs/tex.sty//:"

alias mail='mutt'
alias oldmail="mutt -f =received"
alias adminmail="mutt -f =admin"

alias sgroff='PAGER=${LESSBIN:-more} pagecommand groff -eptsR -Tascii -ms'

# dpkg stuff
alias dpkg--h='PAGER=${LESSBIN:-more} pagecommand dpkg --help'
alias dpkg--all='PAGER=${LESSBIN:-more} pagecommand dpkg -l'
alias dpkg--l='PAGER=${LESSBIN:-more} pagecommand dpkg -L'
alias dpkg--inf='PAGER=${LESSBIN:-more} pagecommand dpkg --info'
alias dpkg--toc='PAGER=${LESSBIN:-more} pagecommand dpkg --contents'

find_in_dpkgs() {
    if [ "$1" = "-L" ]; then
        showContents='y'
        shift
    fi

    for f in "$@"; do
        dpkgS=$(dpkg -S $(which $f))
        if [ -z "$showContents" ]; then
            echo "$dpkgS"
        else
            srcPkg="${dpkgS%%:*}"
            echo "$srcPkg    ($f)"
            echo "======================================================="
            dpkg -L $srcPkg
            echo ""
        fi
    done
}
alias dpkg--whichcontains='PAGER=${LESSBIN:-more} pagecommand find_in_dpkgs'
alias \
    dpkg--l-whichcontains='PAGER=${LESSBIN:-more} pagecommand find_in_dpkgs -L'

# rpm stuff
alias rpm-h='PAGER=${LESSBIN:-more} pagecommand rpm --help'
alias rpm-all='PAGER=${LESSBIN:-more} pagecommand rpm -qa'
alias rpm-qfile='PAGER=${LESSBIN:-more} pagecommand rpm -qp'
alias rpm-upgrade='sudo rpm --upgrade -hv'
alias rpm-install='sudo rpm --install -hv'
whatrequires_rpm() {
    rpm -q --whatrequires \
        `rpm -q --provides "$@" | grep -v " = " | \
             awk '{ print $1 }'`
}
extract_from_rpm() {
    compress="$1"
    suffix="tgz"
    shift
    if [ -z "$*" ]; then compress="--help"; fi
    case "$compress" in
        -z|--gz)
            compress=-z
            suffix="tgz"
            ;;
        -j|--bz|--bz2)
            compress=-j
            suffix="tbz"
            ;;
        -h|--help|*)
            echo -n "extract_from_rpm [-j|--bz2|-z|--gz] "
            echo "pkgfile [files to extract ...]"
            echo
            echo "Extracts to the current working directory."
            return 0
            ;;
    esac
    pkg="$1"
    shift
    tarball=`basename ${pkg%%.rpm}.${suffix}`
    # Note: Conversion fails.  Haven't figured out how to get tar to archive
    # from stdin.
    #echo "Converting package \"$pkg\""
    #echo "to tarball:  $tarball"
    #rpm2cpio ${pkg} | cpio --extract --to-stdout | \
    #    tar $compress -cf $tarball -

    rpm2cpio ${pkg} | cpio --extract --make-directories \
        --preserve-modification-time --no-absolute-filenames "$@"
}

# 2-sided Printing

LPR_PS_US='Letter'
LPR_PS_US_MINMARGIN='Letter'
LPR_PS_US_NOMARGIN='Letter'
r_duplexOpts='-o Duplex=DuplexNoTumble -o OptionDuplex=True'
r_pageSzUS="-o PageSize=$LPR_PS_US -o PageRegion=$LPR_PS_US"
r_pageSzUSMM="-o PageSize=$LPR_PS_US_MINMARGIN "
r_pageSzUSMM="$r_pageSzUSMM -o PageRegion=$LPR_PS_US_MINMARGIN"
r_pageSzUSNM="-o PageSize=$LPR_PS_US_NOMARGIN "
r_pageSzUSNM="$r_pageSzUSMM -o PageRegion=$LPR_PS_US_NOMARGIN"
alias lpr-ps="lpr -P ps $r_pageSzUS $r_duplexOpts"
alias lpr-ps-smallMargin="lpr -P ps $r_pageSzUSMM $r_duplexOpts"
alias lpr-ps-noMargin="lpr -P ps $r_pageSzUSNM $r_duplexOpts"
unset r_duplexOpts r_pageSzUS r_pageSzUSMM r_pageSzUSNM

# runx/xtoolwait aliases
alias lyx="runx lyx -geometry 800x$((${X11_YRES:-786} - 32))+0+0"
alias xephem='runx xephem'
alias gnucash='runx gnucash'
alias gnumeric='runx gnumeric'
alias gv='runx gv'
alias kdecd='runx kscd'
alias konq='runx konqueror --profile webbrowsing'
alias kcalc='runx kcalc'
alias uqbarmon='runx gkrellm -s uqbar'

alias trayclose='eject -t'

# Local networking stuff
alias ssh='ssh -4'

#
# *Admin Stuff*
#

alias smacs="sudo emacs"
alias svi="sudo vim"
alias sv="sudo folded-less.sh"
alias sudoclean="sudo clean.sh"
alias sudorm="sudo safe-rm.sh"


########################################
#
# Startup Commands
#
########################################


ddate
# When run via sudo or su, cd to $HOME
if [ "$PWD" != "$HOME" ]; then
    pushd -n ~ >/dev/null 2>&1
fi



##################
# Local Variables:
# mode: Shell-script
# eval: (sh-set-shell "bash" nil nil)
# End:
