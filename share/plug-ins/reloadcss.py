import GPS
from gps_utils import *
from gi.repository import Gtk

providers = {}


def get_css_provider(buf):
    f = buf.file()
    fname = f.name()
    screen = buf.current_view().pywidget().get_screen()
    p = providers.get(fname, None)
    if not p:
        p = Gtk.CssProvider()
        providers[fname] = p
        Gtk.StyleContext.add_provider_for_screen(screen, p, 1000)
    return p


@interactive("Editor",  name="Apply editor's css to GPS")
def apply_css():
    print "IN APPLY CSS BIATCH"
    buf = GPS.EditorBuffer.get()
    text = buf.get_chars()
    print "CSS : ", text
    p = get_css_provider(buf)
    p.load_from_data(text)
