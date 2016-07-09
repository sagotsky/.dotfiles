#!/bin/bash

# git co and clean first?
cp -R ~/repos/interVIEW /tmp/
rm -rf /tmp/interVIEW/.git
sudo rm -rf ~guest/Desktop/interVIEW
sudo chown -R guest:guest /tmp/interVIEW
sudo mv /tmp/interVIEW ~guest/Desktop
