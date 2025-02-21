(defsystem "swm-screenshot"
  :description "Takes screenshots and stores them as png files"
  :author "Erik P Almaraz, Michael Filonenko"
  :license "GPLv3"
  :depends-on ("stumpwm" 
               "clx" 
               "zpng" 
               "local-time")
  :serial t
  :components ((:file "swm-screenshot")))
