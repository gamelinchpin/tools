alias fv='less'
alias v='less'
alias envv='set | less'
alias cdlast='cd $OLDPWD'
alias cdkboxhome='cd $HOME_KBOX'
alias cd_kbox='cd $KBOX'
alias dfltop=/system/bin/top
alias showmounts='mount | grep -v asec'
alias dfm='df -m | grep -v asec'


[ -z "$SHELL_FUNCTIONS" ] && SHELL_FUNCTIONS=" "
[ -z "$SH_FN_HELP" ] && SH_FN_HELP=" "


add_help() {
  local nuShFn="$1"
  shift
  
  local fnm l
  for fnm in $SHELL_FUNCTIONS; do
    [ "$fnm" = "$nuShFn" ] && return 1
  done
  
  SHELL_FUNCTIONS="$SHELL_FUNCTIONS $nuShFn"
  
  SH_FN_HELP="$SH_FN_HELP
${nuShFn}() $1"
  shift
  
  for l in "$@"; do
    SH_FN_HELP="$SH_FN_HELP
$l"
  done
}


print_help() {
  local oIFS="$IFS"

  echo "| Shell Functions:"
  echo "|"
  
  IFS="
"
  local l
  for l in $SH_FN_HELP; do
    echo -e "|    $l"
  done

  echo "|"
  echo "|"
  echo "| Aliases:"
  echo "|"

  for l in $(alias); do
    echo -e "|    $l"
  done
  
  IFS="$oIFS"
}
alias help='print_help | less'


zv() {
    local opts f cmd
    while [ -n "$1" ]; do
        case "$1" in
            -*)
                opts="$opts $1"
                shift
                ;;
            *)
                break
                ;;
        esac
    done

    for f in "$@"; do
        case "$f" in
            *.gz|*.Z)
                cmd=zcat
                ;;
            *.bz|*.bz2)
                cmd=bzcat
                ;;
            *.lzo)
                cmd=lzopcat
                ;;
            *.lzma|*.7z)
                cmd=lzcat
                ;;
            *.xz)
                cmd=xzcat
                ;;
        esac
       (echo "========================================";\
        echo "=== File:  $f";\
        echo "========================================";\
        $cmd "$f" 2>/dev/null) | less $opts
    done
}
add_help zv \
  "<*.gz|*.bz|*.bz2|*.lzo|*.lzma|*.xz> \\" \
  "     [<*.gz|...> ...]"


dirs() {
    if [ "$1" = "--clear_all" ]; then
        DIRSTACK=""
    fi
    echo $PWD $DIRSTACK
}

popd() {
    set -- $DIRSTACK
    if [ -n "$1" ]; then
        cd "$1"
        shift
        DIRSTACK="$*"
        dirs
    else
        echo "popd():  Directory stack empty."
    fi
}


pushd() {
    local do_cd='y'
    local rotLeftN=''
    local rotRightN=''
    local nuDir=''

    if [ "$1" = "-n" ]; then
        do_cd=""
        shift
    fi

    if [ -z "$*" ]; then

        set -- $DIRSTACK
        if [ -n "$1" ]; then
            nuDir="$1"
            shift
            do_cd='y'
            if [ -n "$*" ]; then
                DIRSTACK="$PWD $*"
            else
                DIRSTACK="$PWD"
            fi
        else
            DIRSTACK="$oldFirst"
            echo "pushd():  No other directory."
            return 1
        fi

    else

        case "$1" in
            +[0-9]*)
                rotLeftN="${1#+}"
                ;;
            -[0-9]*)
                rotRightN="${1#-}"
                ;;
            *)
                nuDir="$1"
                ;;
        esac

        if [ -n "$rotLeftN" ]; then
            if [ $rotLeftN -eq 0 ]; then
                dirs
                return 0
            fi
        fi

        if [ -n "$rotLeftN$rotRightN" ]; then

            if [ -n "$rotRightN" ]; then
                local N=0
                local d
                for d in "$PWD" $DIRSTACK; do
                    N=$((N + 1))
                done
                rotLeftN=$((N - rotRightN - 1))
            fi

            set -- $PWD $DIRSTACK

            local stacktail=''
            while [ -n "$*" -a $rotLeftN -gt 0 ]; do
                stacktail="$stacktail $1"
                shift
                rotLeftN=$((rotLeftN - 1))
            done

            if [ -z "$nuDir" ]; then
                nuDir="$1"
                shift
            fi
            DIRSTACK="$* $stacktail"
        else
            DIRSTACK="$PWD $DIRSTACK"
        fi

    fi

    if [ -n "$do_cd" -a -n "$nuDir" ]; then
        cd $nuDir
    fi
    dirs
}
alias pd=pushd


add_help pushd "{[-n] <+N>|<-N>}|<nuDir>" \
  "    ('-n'==only shift the dirstack; no cd)"
add_help popd "[<nuDir>]"
add_help dirs "[--clear_all]"


tls() {
  local tarfile="$1"
  shift

  local ct
  if [ -n "$1" ]; then
    ct=$tarfile
    tarfile="$1"
  else
    case "${tarfile%%**.}" in
      gz|tgz)
        ct=-z
        ;;
      bz2|tbz)
        ct=-j
        ;;
      lzma)
        ct=-a
        ;;
    esac
  fi
 
  tar $ct -tvf "$tarfile" | less
}
add_help tls "[-z|-j|-a] <tarfile>"


alias tzv='tls -z'
alias tbv='tls -j'


safe_rm() {
  if [ $# -eq 0 ]; then
    return 0
  fi

  local tmpDir=/mnt/sdcard/tmp
  
  if [[ -d /mnt/sdcard/ext/tmp ]]; then
    tmpDir=/mnt/sdcard/ext/tmp
  fi
 
  local purgeDir=$tmpDir/.purged
  if [[ ! -d $purgeDir ]]; then
    mkdir $purgeDir || return 1
  fi

  mv "$@" $purgeDir
}
add_help safe_rm "<files>"

alias saferm='safe_rm'


mkln_s() {
  local orig="$1"
  shift
  local targlink="$1"
  shift
  
  if [ -z "$orig" -o -z "$targlink" ]; then
    echo "usage:  mkln_s <srcFile> <destLink>"
    return 1
  fi
  
  local origBase=`basename "$orig"`
  
  if [[ -d $targlink ]]; then
    targlink="${targlink%%/}/${origBase}"
  fi
  
  if [[ -e $targlink ]]; then
    echo "File exists:  \"$targlink\""
    echo "Move it out of the way first."
    return 2
  fi
  
  mv "$orig" "$targlink" && ln -s "$targlink" "$orig"
}
add_help mkln_s "<file> <destSymlink>"

alias mvlns='mkln_s'

