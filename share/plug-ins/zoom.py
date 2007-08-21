"""This scripts adds a menu for quickly changing font sizes

When this plug-in is loaded, it adds a new menu /Edit/Text Size which
provides a fast access to the preferences related to font sizes. This
can be used to quickly zoom on the text.

The modification is permanent, since it modifies the actual preferences,
and the new text size will be active when GPS is relaunched later on.
"""


#############################################################################
## No user customization below this line
#############################################################################

from GPS import *
import pango

def zoom_pref(pref, factor, save=True):
   p = Preference (pref)
   (font, fg, bg) = p.get().split ("@")
   descr = pango.FontDescription (font)
   descr.set_size (int (descr.get_size() * factor))
   p.set (descr.to_string() + "@" + fg + "@" + bg, save)

def zoom (factor):
   zoom_pref ("Src-Editor-Keywords-Style", factor, False)
   zoom_pref ("Src-Editor-Strings-Style",  factor, False)
   zoom_pref ("Src-Editor-Default-Style",  factor, False)
   zoom_pref ("Src-Editor-Comments-Style", factor, True)

Menu.create ("/Edit/Text Size/Increase Text Size", lambda x: zoom(1.2),
             ref="Preferences", add_before=True)
Menu.create ("/Edit/Text Size/Decrease Text Size", lambda x: zoom(1/1.2))
parse_xml ("""
  <key action="/Edit/Text Size/Increase Text Size">control-plus</key> 
  <key action="/Edit/Text Size/Decrease Text Size">control-minus</key> 
""")
