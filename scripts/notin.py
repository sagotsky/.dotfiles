#!/usr/bin/python

"""notin': A simple org.freedesktop.Notification listener."""

import gobject

import dbus
import dbus.service
import dbus.mainloop.glib

import sys

class Notin(dbus.service.Object):
    last = '' 

    def __init__(self, bus, object_path):
        dbus.service.Object.__init__(self, bus, object_path)

    @dbus.service.method("org.freedesktop.Notifications",
                         out_signature='as', in_signature='')
    def GetCapabilities(self):
        return ["body"]

    @dbus.service.method("org.freedesktop.Notifications",
                         out_signature='u', in_signature='susssasa{sv}u')
    def Notify(self, app_name, replaces_id, app_icon, summary, body, actions,
            hints, expire_timeout):
        notification = {"app_name": app_name,
                        "replaces_id": replaces_id,
                        "app_icon": app_icon,
                        "summary": summary,
                        "body": body,
                        "actions": actions,
                        "hints": hints,
                        "expire_timeout": expire_timeout}

        if notification['body'] != self.last:
          print (u"[%(app_name)s] %(summary)s: %(body)s" % notification).encode('utf-8')
          sys.stdout.flush()
          self.last = notification['body']

        return 1

    @dbus.service.method("org.freedesktop.Notifications",
                         out_signature='', in_signature='u')
    def CloseNotification(self, notification_id):
        #self.queue.dequeue(notification_id)
        self.NotificationClosed(notification_id, 1)
        return

    @dbus.service.method("org.freedesktop.Notifications",
                         out_signature='ssss', in_signature='')
    def GetServerInformation(self):
        return "notin'", "Tordek", "0.0.1", "1.2"

    @dbus.service.signal("org.freedesktop.Notifications",
                         signature='uu')
    def NotificationClosed(self, id, reason):
        return

def main():
    dbus.mainloop.glib.DBusGMainLoop(set_as_default=True)

    system_bus = dbus.SessionBus()

    name = dbus.service.BusName("org.freedesktop.Notifications",
            system_bus, replace_existing=True, do_not_queue=False)
    obj = Notin(system_bus, '/org/freedesktop/Notifications')

    mainloop = gobject.MainLoop()

    mainloop.run()

if __name__ == '__main__':
    main()
