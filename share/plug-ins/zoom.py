"""This scripts adds a menu for quickly changing font sizes

When this plugin is loaded, it adds a new menu /Edit/Text Size which
provides a fast access to the preferences related to font sizes. This
can be used to quickly zoom on the text.

The modification is permanent, since it modifies the actual preferences,
and the new text size will be active when GPS is relaunched later on.
"""

#############################################################################
# No user customization below this line
#############################################################################

from GPS import *
from gi.repository import Pango
import gps_utils


def zoom_pref(pref, factor, save=True):
    p = Preference(pref)
    (font, fg, bg) = p.get().split("@")
    descr = Pango.FontDescription(font)
    descr.set_size(int(descr.get_size() * factor))
    p.set(descr.to_string() + "@" + fg + "@" + bg, save)


def zoom(factor):
    zoom_pref("Src-Editor-Reference-Style", factor, True)


@gps_utils.interactive(name="increase text size", category="Editor")
def zoom_in():
    """Increase the size of fonts in the source editors.
This impacts the corresponding preferences."""
    zoom(1.2)


@gps_utils.interactive(name="decrease text size", category="Editor")
def zoom_out():
    """Decrease the size of fonts in the source editors.
This impacts the corresponding preferences."""
    zoom(1 / 1.2)
