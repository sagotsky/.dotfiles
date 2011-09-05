#!/bin/sh

# Creates a new db and a user for it, with pw=useruseruser!

if [ $# -eq 1 ] ; then
    USER=$1
else
    echo "usage: $0 NAME_OF_USER"
    exit 1
fi

echo -n "root@MySQL "
mysql -u root -p <<EndSQL
CREATE DATABASE $USER;
CREATE USER '$USER'@'localhost' IDENTIFIED BY '$USER$USER$USER!';
GRANT ALL PRIVILEGES ON $USER.* TO '$USER'@'localhost';
FLUSH PRIVILEGES;
EndSQL

if [ $? -eq 0 ] ; then
    echo "Created db $USER and user $USER"
    echo $USER | xsel
else
    echo "Could not create db $USER"
fi


