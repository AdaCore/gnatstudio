"""
Check that GPS highlights gnatpp output in Messages view.
"""
import GPS
from gs_utils.internal.utils import *

@run_test_driver
def test_driver():
    prefix = 'Messages:Custom Highlighting '
    color = 'DEFAULT@rgb(164,0,0)@rgba(0,0,0,0)'
    GPS.Preference(prefix + '1/regexp').set('^gnat[a-z ]*')
    GPS.Preference(prefix + '1/variant').set(color)
    GPS.Preference(prefix + '2/regexp').set('process terminated')
    GPS.Preference(prefix + '2/variant').set(color)
    f = GPS.File('unit.adb')
    b = GPS.EditorBuffer.get(f)
    GPS.execute_action('pretty print')
    yield wait_tasks()

    msg = GPS.Console("Messages").pywidget()
    tv = get_widgets_by_type(Gtk.TextView, msg)[0]
    at = tv.get_buffer()
    it = at.get_start_iter ()
    tag_list = []

    while it.forward_to_tag_toggle():
        tag_list.append ('%d:%d'
            % (it.get_line() + 1, it.get_line_offset() + 1))

    expect = ['4:1', '4:13', '7:23', '7:41', '8:1']
    gps_assert(tag_list, expect, "Wrong highlighting")
