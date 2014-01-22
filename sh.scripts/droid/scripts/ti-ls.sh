#alias ti-ls="ls | grep -v '\(com\.\(android\|google\|htc\)\|mobi\.mgeek\|tritop\)'"

tistash()
{
  mv "$@" /sdcard/Backups/stash/withTiBaks/
}

tils()
{
  ls "$@" |\
    grep -v '\(com\.\(android\|google\|htc\)\|mobi\.mgeek\|tritop\)'
}

