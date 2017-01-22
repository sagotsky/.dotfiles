#!/bin/bash

echo "Gimme sudo"
sudo echo "I haz teh sudo"

cd /tmp/
git clone git@github.com:patientslikeme/interVIEW.git
rm -rf interVIEW/.git
sudo chown -R guest:guest interVIEW
sudo rm -rf ~guest/Desktop/interVIEW
sudo mv interVIEW ~guest/Desktop/
