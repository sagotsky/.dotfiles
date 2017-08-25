#!/bin/bash

echo 'vpn...'
systemctl status openvpn-client@plm --quiet || xterm -e sudo systemctl start openvpn-client@plm
