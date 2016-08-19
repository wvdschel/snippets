setopt prompt_subst
ORIG_PS1=${PS1}
ORIG_RPS1=${RPS1}
PS1='%1{$(if [[ $? == 0 ]]; then echo -e "\033[0;32m\xE2\x9C\x94"; else echo -e "\033[0;31m\xE2\x9C\x95"; fi )$(echo -e "\033[0;0m")%} ${ORIG_PS1} '
RPS1='[${DCM_IP} ${ORIG_RPS1}]'
