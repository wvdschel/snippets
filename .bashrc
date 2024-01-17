export PATH=~/bin:~/go/bin:$PATH
# Customized PS1 prompt
PS_NORMAL="[\u@\h \w]"
prompt() {
    EXIT_CODE=$?
    LAST_EXIT_STRING="$(if [[ $EXIT_CODE == 0 ]]; then echo -en "\[\033[0;32m\]\xE2\x9C\x94"; else echo -en "\[\033[0;31m\]\xE2\x9C\x95"; fi; echo -en "\[\033[0;0m\]")"
    notify_done $EXIT_CODE
    # Set current working directory as terminal title
    echo -ne "\033]0;${PWD/${HOME}/~}\007"
    if [ -z $DCM_IP ]; then
	PS_SUFFIX="[`date +%H:%M:%S`]"
    else
        PS_SUFFIX="[DCM: $DCM_IP] [`date +%H:%M:%S`]"
    fi
    width=`expr "$(tput cols)" + 0` # Because printf counts color codes as characters, we need to add some extra padding
    PS1=$(printf "$(tput smul)%*s\r$(tput smul)%s$(tput sgr0)\n%s " "${width}" "$PS_SUFFIX" "$PS_NORMAL" "$LAST_EXIT_STRING")
}

function chd {
  if ! [ -z $2 ]; then
    cd $(pwd | sed -e "s|$1|$2|")
  else
    cd $(pwd | sed -e "s|$1.*|$1|") 
  fi
}

function retry()
{
    local RETRY_COUNT=$1
    local TIMEOUT=$2
    local PAUSE=$3
    shift; shift; shift
    local CMD=("$@")

    echo "Retrying '${CMD[@]}' at most ${RETRY_COUNT} times with $TIMEOUT seconds per attempt, in $PAUSE second intervals."
    
    local n=0
    until [ $n -ge $RETRY_COUNT ]
    do
        echo Attempt $n
        timeout -s 9 $TIMEOUT "${CMD[@]}" && break
        n=$[$n+1]
        sleep $PAUSE
    done
    if [ $n -eq $RETRY_COUNT ]
    then
        echo "Error: ${CMD[@]} failed. Exiting."
        return 1
    fi

    return 0
}

# Stolen from https://superuser.com/questions/175799/does-bash-have-a-hook-that-is-run-before-executing-a-command#175802
preexec_invoke_exec () {
    [ -n "$COMP_LINE" ] && return  # do nothing if completing
    [ "$BASH_COMMAND" = "$PROMPT_COMMAND" ] && return # don't cause a preexec for $PROMPT_COMMAND
    local this_command=`HISTTIMEFORMAT= history 1 | sed -e "s/^[ ]*[0-9]*[ ]*//"`;
    preexec "$this_command"
}
trap 'preexec_invoke_exec' DEBUG

function notify_done()
{
    local EXIT_CODE=$1
    local LAST_CMD=`HISTTIMEFORMAT= history 1 | sed -e "s/^[ ]*[0-9]*[ ]*//"`;
    local TIME=$(date +%s)
    local NOTIFY=$(which notify-send 2> /dev/null)
    if ! [ -z $START_TIME ] &&
            ! [ -z $NOTIFY ] &&
            [ $TIME -gt $(( $START_TIME + 30 )) ]; then
        if [ $EXIT_CODE -eq 0 ]; then
    	    ($NOTIFY -t $(( $TIME + 3 )) -a "Terminal" -c transfer.complete "Command finished" "$LAST_CMD" 2>/dev/null &)
        else
    	    ($nOTIFY -t $(( $TIME + 3 )) -a "Terminal" -i error -c transfer.complete "Command failed ($EXIT_CODE)" "$LAST_CMD" 2>/dev/null &)
        fi
    fi
    unset START_TIME
}

function preexec()
{
    START_TIME=$(date +%s)
}

function _cdp_completions()
{
    if [ "${#COMP_WORDS[@]}" != "2" ]; then
        return
    fi
    while read i
    do
        if [[ "$(basename "$i")" =~ ^"${COMP_WORDS[1]}" ]]; then
                COMPREPLY+=($(basename "$i"))
        fi
    done <<<"$(find ~/Unison/Code -maxdepth 2 -type d -not -name '.*' -print0 | xargs -0 -i echo "{}")"
}

function cdp()
{
    local newdir=""
    newdir=$(find ~/Unison/Code -maxdepth 2 -type d -not -name '.*' -print0 | xargs -0 -i echo "{}" | while read i
    do
        if [[ "$(basename "$i")" == "$1" ]]; then
                echo "$i"
        fi
    done)
    if ! [[ -z "$newdir" ]]; then
        cd "$newdir"
    fi
}

complete -F _cdp_completions cdp

unset START_TIME
source $HOME/.cargo/env
alias syncup=~/Unison/sync.sh
PROMPT_COMMAND=prompt
source /usr/share/bash-completion/bash_completion 

# On OpenSUSE, sbin is not part of PATH by default
#export PATH=$PATH:/sbin:/usr/sbin
#export NDK_HOME=~/Apps/android-ndk-r12b
#export ANDROID_HOME=~/Apps/android-sdk-linux
#export PATH=$ANDROID_HOME/tools:$PATH
. "$HOME/.cargo/env"
export EDITOR=hx
