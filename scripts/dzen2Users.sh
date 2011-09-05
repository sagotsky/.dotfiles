#!/bin/sh

# lists logged in users +new login in a dzen2 menu.  clicking on a user runs gdmSwitch.sh $USERNAME

USERS=$(gdmflexiserver -a -c "ALL_SERVERS" | sed -e 's/[:;]/\n/g' | grep , | sed -e 's/[0-9]*,//')
SWITCHER="/home/sagotsky/scripts/gdmSwitch.sh"

for u in $USERS ; do
    echo "$u : $SWITCHER $u"
done

echo "-- New Login : $SWITCHER fakeaccountthatdoesntreallyexistgetsanewlogin"
