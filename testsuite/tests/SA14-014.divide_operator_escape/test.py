"""
This test checks that "/" operator is escaped correctly in
contextual menu.
"""
import GPS
from gs_utils.internal.utils import *

@run_test_driver
def test_driver():
    b = GPS.EditorBuffer.get(GPS.File("p.ads"))
    b.current_view().goto(b.at(4, 14))
    yield timeout(500)
    w = Gtk.Window.list_toplevels()
    click_in_text(GPS.EditorBuffer.get().current_view().cursor(), button=3)
    m = get_contextual(w)
    d = dump_contextual(w)
    close_contextual(w)

    gps_assert(['/ is called by'] in d, True,
               "wrong menu content {}".format(d))
