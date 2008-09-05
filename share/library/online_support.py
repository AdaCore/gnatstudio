"""This plug-in adds new menus that provides easy easy access to the AdaCore
on-line support services:

  - Viewing your support tickets
  - Opening new tickets
  - Browsing on-line documentation
  - Downloading GNAT Pro and wavefronts
  - Browsing various developer resources
  - Viewing the Ada Gem of the Week
  - Finding an intern

All these facilities are available from the /Help/On-Line Support menu.
"""


###########################################################################
## No user customization below this line
###########################################################################

import GPS

GPS.Preference ("Plugins/online_support/gap").create (
  "GAP member", "boolean",
  """If you are an academic user member of the GAP program, enable this preference. This slightly changes the menus to give you better access to GnatTracker.
You need to restart GPS after changing this preference""",
  False)

def open_gnattracker(uri):
   GPS.HTML.browse ("http://www.adacore.com/gnattracker" + uri)

def view_ticket (menu):
   ticket=GPS.MDI.input_dialog ("View a ticket", "ticket number")
   if len (ticket) == 1 and ticket [0] != "":
      open_gnattracker ("/ticket?tn=" + ticket[0])

def on_gps_started (hook):
   gap_member = GPS.Preference ("Plugins/online_support/gap").get()
   GPS.Menu.create ("/Help/On-Line Support/Dashboard", \
                    lambda x: open_gnattracker (""))
   GPS.Menu.create ("/Help/On-Line Support/View ticket", view_ticket)
   GPS.Menu.create ("/Help/On-Line Support/New ticket", \
                    lambda x: open_gnattracker ("/email"))
   GPS.Menu.create ("/Help/On-Line Support/Download", \
                    lambda x: open_gnattracker ("/gnatpro"))
   if gap_member:
      GPS.Menu.create ("/Help/On-Line Support/GAP Contributions", \
                       lambda x: open_gnattracker ("/contributions"))
   else:
      GPS.Menu.create ("/Help/On-Line Support/Wavefronts", \
                       lambda x: open_gnattracker ("/wavefronts"))
   GPS.Menu.create ("/Help/On-Line Support/Documentation", \
                    lambda x: open_gnattracker ("/docs"))
   GPS.Menu.create ("/Help/On-Line Support/Developers Center", \
                    lambda x: GPS.HTML.browse \
                      ("http://www.adacore.com/category/developers-center"))
   GPS.Menu.create ("/Help/On-Line Support/Ada Gem of the Week", \
                    lambda x: GPS.HTML.browse \
                      ("http://www.adacore.com/home/ada_answers/gems"))
   if gap_member:
      GPS.Menu.create ("/Help/On-Line Support/Ada Intern Program", \
                       lambda x: open_gnattracker ("/gap_aip"))
   else:
      GPS.Menu.create ("/Help/On-Line Support/Ada Intern Program", \
                       lambda x: open_gnattracker ("/aip"))

GPS.Hook ("gps_started").add (on_gps_started)
