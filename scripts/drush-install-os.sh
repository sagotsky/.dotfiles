#!/bin/sh

HOST=$1

drush $HOST si -y openscholar \
  --account-name=admin \
  --account-pass=password \
  --site-name=$HOST \
  openscholar_flavor_form.os_profile_flavor=development \
  openscholar_install_type.os_profile_type=vsite

drush $HOST uli
echo -e "\a"
