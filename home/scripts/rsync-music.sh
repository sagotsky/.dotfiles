#!/bin/bash

DIR=/media/raid/music
BACKUP=/media/extra/

rsync -avz $DIR $BACKUP
