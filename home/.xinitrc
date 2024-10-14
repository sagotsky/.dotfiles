# Todo: move common xprofile stuff in here

echo "" > ~/.xsession-errors
echo
echo --------------------
date
echo $SHELL
echo

echo "${HOME}/.xinitrc.$(hostname)"
${HOME}/.xinitrc.$(hostname)

# vim: set ft=shell
