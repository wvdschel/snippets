#!/bin/bash

CLOCKFACES="🕐🕛🕧🕜🕑🕝🕒🕞🕓🕟🕔🕠🕕🕡🕖🕢🕗🕣🕘🕤🕙🕥🕚🕦"

HOUR=$(( $(date +%H) % 12 ))
MIN=$(date +%M)

CHARACTER_INDEX=$(( ( $HOUR * 100 + $MIN * 50 / 30 ) / 50 ))
echo "${CLOCKFACES:$CHARACTER_INDEX:1}"
