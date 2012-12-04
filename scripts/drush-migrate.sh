#!/bin/sh

HOST=$1

drush $HOST mi --group=tablecopy
drush $HOST mi UsersOS
drush $HOST mi BookTableOS,MenuLinksTableOS,ModalImageNodeOS
drush $HOST mi --group=specialtables
drush $HOST mi --group=inline
drush $HOST mi VsiteNodeOS
drush $HOST mi --group=node
drush $HOST mi --group=comments
drush $HOST mi --group=taxonomy
drush $HOST mi SpacesOverridesBoxesMedia
drush $HOST mi --group=spacesoverrides
drush $HOST mi ThemeOS,OgUsersOS
drush $HOST mi --all

