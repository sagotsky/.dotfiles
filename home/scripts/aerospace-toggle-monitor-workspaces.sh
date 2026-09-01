#!/usr/bin/env bash

nth() {
	N=${1:-1}
	awk "{print \$$N}"
}

focused_monitor() { aerospace list-monitors --focused | nth ;}
unfocused_monitors() { aerospace list-monitors | grep -v "^$(focused_monitor) " | nth ;}
