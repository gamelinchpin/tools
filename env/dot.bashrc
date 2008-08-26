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
if [ -n "$INTERACTIVE" -a -n "$BASHRC_INTERACTIVE" ]; then
    . $BASHRC_INTERACTIVE
fi

# Punt if non-interactive.
unset BASHRC_INTERACTIVE
if [ -z "${INTERACTIVE}" ]; then
    return
fi



#############################################################################
#
# User-Defined Aliases, Variables, Functions
#
# Place your personal settings after this comment.
#############################################################################



STTYPE="${MACHTYPE}-${OSTYPE}"

export NAME="John Weiss"
export PATH="${PATH}:/usr/X11R6/bin:${HOME}/bin"


alias edit="runemacs.sh"
alias tbv='PAGER=/usr/bin/less pagecommand tar -jtvf'
alias untar='tar -zxvf'
alias untarb='tar -jxvf'
alias tarzf='tar -zcvf'
alias tarbf='tar -jcvf'
alias veryclean="(cd ; clean -r)"

alias repath="export PATH=${PATH}"
alias resharc='source ~/.bashrc'

alias twoup='enscript -2jv -f Courier-Bold7 --landscape'
alias odx='od -A x -t x1z'

# CVS Stuff
#
alias cvsmerge='cvs update -kk -j'
alias cvsmerge-head='cvs update -kk -j HEAD'
case ${OSTYPE} in
    [Cc]ygwin)
        return
        ;;
esac

#######################################################################
#
# Non-Cygwin Aliases/Vars below this line.
#
#######################################################################

export CVSROOT=${HOME}/src/0_Local_CVS_Repos
export LV="-lic -E'runemacs.sh +%d'"
export PRINTER="ps"
export BIBINPUTS="${HOME}/docs/refs:"
export BSTINPUTS="${BIBINPUTS}:"
export TEXINPUTS=".//:${HOME}/docs/tex.sty//:"
export BROWSER_REMOTE_CTRL=/usr/local/firefox/mozilla-xremote-client

alias mail='mutt'
alias oldmail="mutt -f =received"
alias adminmail="mutt -f =admin"
alias lyxl='mutt -f =lyx.incoming'
alias boostl='mutt -f =boost.incoming'
alias accul='mutt -f =accu.incoming'

alias sgroff='PAGER=${LESSBIN:-more} pagecommand groff -eptsR -Tascii -ms'

      alias latin1='luit -encoding ISO8859-1 -c'
alias latin15='luit -encoding ISO8859-15 -c'

# dpkg stuff
alias dpkg-h='PAGER=${LESSBIN:-more} pagecommand dpkg --help'
alias dpkg-all='PAGER=${LESSBIN:-more} pagecommand dpkg -l'
alias dpkg-l='PAGER=${LESSBIN:-more} pagecommand dpkg -L'
alias dpkg-inf='PAGER=${LESSBIN:-more} pagecommand dpkg --info'
alias dpkg-toc='PAGER=${LESSBIN:-more} pagecommand dpkg --contents'

# rpm stuff
alias rpm-h='PAGER=${LESSBIN:-more} pagecommand rpm --help'
alias rpm-all='PAGER=${LESSBIN:-more} pagecommand rpm -qa'
alias rpm-qfile='PAGER=${LESSBIN:-more} pagecommand rpm -qp'
alias rpm-upgrade='sudo rpm --upgrade -hv'
alias rpm-install='sudo rpm --install -hv'
whatrequires-rpm() {
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
    rpm2cpio ${pkg} | cpio --extract --make-directories \
        --preserve-modification-time --no-absolute-filenames "$@"
}

# admin stuff
alias smacs="sudo emacs"
alias svi="sudo vi"
alias sv="sudo folded-less.sh"
alias sudoclean="sudo purge-files.sh"
alias uqbarcon='xtoolwait xterm -e sudo minicom -l -w --color=on consoleS0' # -t linux-lat

# xtoolwait aliases
alias lyx="xtoolwait lyx -geometry 800x$((${X11_YRES:-786} - 48))+0+0"
alias xephem='xtoolwait xephem'
alias gnucash='xtoolwait gnucash'
alias gnumeric='xtoolwait gnumeric'
alias gv='xtoolwait gv'
alias kdecd='xtoolwait kscd'
alias konq='xtoolwait konqueror --profile webbrowsing'
alias kcalc='xtoolwait kcalc'
alias uqbarmon='xtoolwait gkrellm -s uqbar'
#alias xanim='xanim +f +Zr +Av100 +Gd2.0 \!* ; reset-mixer.sh'

alias xanim='xanim +f +Zr +Av70 +Gd1.5'
alias mplayer='artsdsp mplayer'
alias trayclose='eject -t'

# Local networking stuff
alias ssh='ssh -4'
alias netoff='drainq.sh; ssh -4 inetctrl@uqbar /usr/local/bin/closenet.sh -dsl'
alias netoff_NOW='ssh -4 inetctrl@uqbar /usr/local/bin/closenet.sh -dsl'
alias neton='ssh -4 inetctrl@uqbar /usr/local/bin/netlogin.sh -dsl'


ddate
