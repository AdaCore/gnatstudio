import GPS
from gps_utils import *

xml_conf = """
<key action="Add multi cursor and go down">shift-alt-Down</key>
<key action="Add multi cursor and go up">shift-alt-Up</key>

<key action="Move to next word">control-Right</key>
<key action="Move to previous word">control-Left</key>

<key action="Move to previous character">Left</key>
<key action="Move to next character">Right</key>

<key action="Move to previous line">Up</key>
<key action="Move to next line">Down</key>

<key action="Delete word forward" >control-Delete</key>

<key action="Remove all multi cursors" >control-Escape</key>

<key action="goto beginning of line" >Home</key>
<key action="goto end of line" >End</key>
"""

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

GPS.parse_xml(xml_conf)
