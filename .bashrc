# Customized PS1 prompt
PS_NORMAL="[\u@\h \w]"
prompt() {
	# Set current working directory as terminal title
	echo -ne "\033]0;${PWD/${HOME}/~}\007"
	
        LAST_EXIT_STRING="$(if [[ $? == 0 ]]; then echo -en "\[\033[0;32m\]\xE2\x9C\x94"; else echo -en "\[\033[0;31m\]\xE2\x9C\x95"; fi; echo -en "\[\033[0;0m\]")"
	if [ -z $DCM_IP ]; then
		PS_SUFFIX="[`date +%H:%M:%S`]"
	else
        	PS_SUFFIX="[DCM: $DCM_IP] [`date +%H:%M:%S`]"
	fi
        width=`expr "$(tput cols)" + 0` # Because printf counts color codes as characters, we need to add some extra padding
        PS1=$(printf "$(tput smul)%*s\r$(tput smul)%s$(tput sgr0)\n%s " "${width}" "$PS_SUFFIX" "$PS_NORMAL" "$LAST_EXIT_STRING")
}
PROMPT_COMMAND=prompt

# On OpenSUSE, sbin is not part of PATH by default
export PATH=$PATH:/sbin:/usr/sbin
export NDK_HOME=~/Apps/android-ndk-r12b
export ANDROID_HOME=~/Apps/android-sdk-linux
export PATH=$ANDROID_HOME/tools:$PATH

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
