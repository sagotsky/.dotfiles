#!/bin/bash

echo 'Unlocking postgres...'
sudo mount /var/storage
sudo mount /var/lib/postgresql && sudo service postgresql start
