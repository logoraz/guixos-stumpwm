(defsystem "swm-notify"
  :description "DBus-based notification server part"
  :author "Erik Almaraz <erikalmaraz@fastmail.com, Slava Barinov <rayslava@gmail.com>, "
  :license "GPLv3"
  :serial t
  :depends-on ("stumpwm"
               "xml-emitter"
               "dbus"
               "bordeaux-threads")
  :components ((:file "swm-notify")))
