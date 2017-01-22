#!/usr/bin/python

# connects to dbus and rates rhythmbox songs
import dbus, sys, time

bus = dbus.SessionBus()

# Connect to player
proxy_obj = bus.get_object('org.gnome.Rhythmbox', '/org/gnome/Rhythmbox/Player')
player = dbus.Interface(proxy_obj, 'org.gnome.Rhythmbox.Player')
rbshellobj = bus.get_object('org.gnome.Rhythmbox', '/org/gnome/Rhythmbox/Shell')
rbshell = dbus.Interface(rbshellobj, 'org.gnome.Rhythmbox.Shell')

def setRating(rating):
	currentsong = player.getPlayingUri();
	if currentsong is not None and currentsong != '':
		rbshell.setSongProperty(currentsong, 'rating',  dbus.Double(rating, variant_level=0))

if len(sys.argv) != 2:
	print "RBrate.py takes exactly one argument"
	sys.exit(-1)

# why doesn't python have case switches?
try:
	if sys.argv[1] == '1':
		setRating(1.0)
	elif sys.argv[1] == '2':
		setRating(2.0)
	elif sys.argv[1] == '3':
		setRating(3.0)
	elif sys.argv[1] == '4':
		setRating(4.0)
	elif sys.argv[1] == '5':
		setRating(5.0)
	player.playPause(True)
        time.sleep(0.4)
	player.playPause(True)
# play/pause twice to refresh rating.  may cause lag.  dunno if that's acceptable.
# doing it twice should return to playing/nonplaying status.


except dbus.exceptions.DBusException, (strerror):
	print 'error: %s' % (strerror)
	sys.exit(-1)
