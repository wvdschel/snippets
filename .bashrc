# Customized PS1 prompt
PS_NORMAL="[\u@\h \w]"
prompt() {
        LAST_EXIT_STRING="$(if [[ $? == 0 ]]; then echo -en "\[\033[0;32m\]\xE2\x9C\x94"; else echo -en "\[\033[0;31m\]\xE2\x9C\x95"; fi; echo -en "\[\033[0;0m\]")"
        PS_SUFFIX="[DCM: $DCM_IP] [`date +%H:%M:%S`]"
        width=`expr "$(tput cols)" + 0` # Because printf counts color codes as characters, we need to add some extra padding
        PS1=$(printf "$(tput smul)%*s\r$(tput smul)%s$(tput sgr0)\n%s " "${width}" "$PS_SUFFIX" "$PS_NORMAL" "$LAST_EXIT_STRING")
}
PROMPT_COMMAND=prompt

function chd {
  if ! [ -z $2 ]; then
    cd $(pwd | sed -e "s|$1|$2|")
  else
    cd $(pwd | sed -e "s|$1.*|$1|") 
  fi
}
