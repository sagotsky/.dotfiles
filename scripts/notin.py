#!/usr/bin/python

"""notin': A simple org.freedesktop.Notification listener."""

import gobject

import dbus
import dbus.service
import dbus.mainloop.glib

import sys
import random

class MessageQueue(object):
    def __init__(self):
        self.messages = {}
        self.queue = []
        self.current_message = None

    def enqueue(self, replaces_id, message):
        if replaces_id in self.messages:
            self.messages[replaces_id] = message
            return replaces_id

        key = random.randint(0, 65535)

        while key in self.messages:
            key = random.randint(0, 65535)

        self.messages[key] = message
        self.queue.append(key)

        return key

    def dequeue(self, message_id):
        if message_id in self.queue:
            self.queue.remove(message_id)

        if message_id in self.messages:
            del self.messages[message_id]

    def expired_message(self, message_id, delta):
        if message_id not in self.messages:
            return True

        message = self.messages[message_id]

        if message["expire_timeout"] != 0:
            if message["expire_timeout"] == -1:
                message["expire_timeout"] = 5000

            message["expire_timeout"] -= delta
            if message["expire_timeout"] <= 0:
                return True

        return False

    def update(self, delta):
        if not (self.current_message or self.queue):
            return True

        if self.current_message:
            if self.expired_message(self.current_message, delta):
                self.dequeue(self.current_message)
                self.current_message = None
                if not self.queue:
                    print
                    sys.stdout.flush()

        if self.queue:
            if self.current_message:
                self.queue.append(self.current_message)

            self.current_message = self.queue.pop(0)
            print (u"[%(app_name)s] %(summary)s: %(body)s" %
                    self.messages[self.current_message]).encode('utf-8')
            sys.stdout.flush()


        return True


class Notin(dbus.service.Object):
    def __init__(self, queue, bus, object_path):
        dbus.service.Object.__init__(self, bus, object_path)
        self.queue = queue

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

        return self.queue.enqueue(replaces_id, notification)

    @dbus.service.method("org.freedesktop.Notifications",
                         out_signature='', in_signature='u')
    def CloseNotification(self, notification_id):
        self.queue.dequeue(notification_id)
        self.NotificationClosed(notification_id, 3)
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

    queue = MessageQueue()
    system_bus = dbus.SessionBus()

    name = dbus.service.BusName("org.freedesktop.Notifications",
            system_bus, replace_existing=True, do_not_queue=True)
    obj = Notin(queue, system_bus, '/org/freedesktop/Notifications')

    mainloop = gobject.MainLoop()

    gobject.timeout_add(1000, lambda: queue.update(1000))
    mainloop.run()

if __name__ == '__main__':
    main()
