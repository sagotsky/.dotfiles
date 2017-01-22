#!/bin/bash

eval $(keychain -q --eval)
nmcli c up id 'VPN patientslikeme'

cd ~/source/devdash
#scp -C stage1:/usr/local/pgsql/developer_db.pgdump /tmp/
scp -C developer@10.208.0.207:developer_db.pgdump /tmp/
#bundle exec devdash download_db
mv /tmp/developer_db* /var/storage/
