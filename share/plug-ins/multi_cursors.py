import GPS
from gps_utils import *

@interactive ("Editor",  name="Remove all multi cursors")
def remove_all_multi_cursors ():
    buffer = GPS.EditorBuffer.get()
    buffer.remove_all_multi_cursors()

@interactive ("Editor", name="Add multi cursor and go down")
def mc_down ():
    buffer = GPS.EditorBuffer.get()
    view = buffer.current_view()
    loc = view.cursor()
    buffer.add_multi_cursor (loc)

    buffer.set_multi_cursors_manual_sync()
    view.goto (GPS.EditorLocation(buffer, loc.line()+1, loc.column()))
    buffer.set_multi_cursors_auto_sync()

@interactive ("Editor", name="Add multi cursor and go up")
def mc_up ():
    buffer = GPS.EditorBuffer.get()
    view = buffer.current_view()
    loc = view.cursor()
    buffer.add_multi_cursor (loc)

    buffer.set_multi_cursors_manual_sync()
    view.goto (GPS.EditorLocation(buffer, max(loc.line()-1,1), loc.column()))
    buffer.set_multi_cursors_auto_sync()
