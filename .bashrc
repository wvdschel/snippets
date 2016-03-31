# Customized PS1 prompt
PS_NORMAL="[\u@\h \w]"
prompt() {
        LAST_EXIT_STRING="$(if [[ $? == 0 ]]; then echo -e "\[\033[0;32m\]\xE2\x9C\x94"; else echo -e "\[\033[0;31m\]\xE2\x9C\x95"; fi )"
        PS_SUFFIX="[`date +%H:%M:%S`] ${LAST_EXIT_STRING}\[\033[0m\]"
        width=`expr "$(tput cols)" + 24` # Because printf counts color codes as characters, we need to add some extra padding
        PS1=$(printf "$(tput smul)%*s\r$(tput smul)%s$(tput sgr0)\n\$ " "${width}" "$PS_SUFFIX" "$PS_NORMAL")
}
PROMPT_COMMAND=prompt
