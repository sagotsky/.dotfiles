#!/bin/bash

eval $(keychain -q --eval)
cd ~/source/devdash
scp -C stage1:/usr/local/pgsql/developer_db.pgdump /tmp/
#bundle exec devdash download_db
mv /tmp/developer_db* /var/storage/