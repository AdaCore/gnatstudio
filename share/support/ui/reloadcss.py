"""
This plugin provides a convenient action to force the reload
of the CSS file.
This is mostly useful for developers who want to build their
own CSS files.
"""

import GPS
from gps_utils import interactive
from gi.repository import Gtk

providers = {}


def get_css_provider(buf):
    # ??? Why do we need one provider per buffer ? One per
    # screen who be enough and more efficient

    f = buf.file()
    fname = f.name()
    p = providers.get(fname, None)
    if not p:
        p = Gtk.CssProvider()
        providers[fname] = p
        screen = buf.current_view().pywidget().get_screen()
        Gtk.StyleContext.add_provider_for_screen(screen, p, 1000)
    return p


@interactive("Editor", name="Apply editor's css to GPS")
def apply_css():
    buf = GPS.EditorBuffer.get()
    text = buf.get_chars()
    p = get_css_provider(buf)
    p.load_from_data(text)
