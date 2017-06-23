"""This file makes every other line read-only.
   This is an example of using overlays"""

# ??? For a real production script, this script should be based
# on gps_utils.highlighter.py, which provides on-the-fly highlighting

from GPS import parse_xml, EditorBuffer

parse_xml("""
  <action name="make lines readonly">
     <shell lang="python">readonly.make_readonly()</shell>
  </action>
  <menu action="make lines readonly">
     <title>/Tests/Make Lines Readonly</title>
  </menu>
""")


def make_readonly():
    """Make every other line readonly in the current file"""
    buffer = EditorBuffer.get()
    loc = buffer.at(1, 1)

    overlay = buffer.create_overlay("readonly")
    overlay.set_property("editable", False)

    while loc < buffer.end_of_buffer():
        eol = loc.end_of_line()
        buffer.apply_overlay(overlay, loc, eol - 1)
        loc = loc.forward_line(2)
