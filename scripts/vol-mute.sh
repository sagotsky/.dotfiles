#!/bin/sh 

amixer sset Master  toggle  > /dev/null

touch /dev/snd/controlC0 
