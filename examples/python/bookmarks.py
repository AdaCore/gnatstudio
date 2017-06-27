# This file adds support for bookmarks in GPS
# Missing in this very partial implementation:
#   - Add known bookmarks to a menu dynamically, for easy jumps

# This is mostly reimplemented in GPS itself at this point

import GPS

GPS.Editor.register_highlighting("bookmarks", "orange", True)


def add_bookmark():
    context = GPS.current_context()
    if isinstance(context, GPS.FileContext):
        name = GPS.MDI.input_dialog("Name of bookmark", "Name")
        if name != ():
            GPS.Locations.add("Bookmarks", context.file(),
                              GPS.Editor.cursor_get_line(
                                  context.file().name()),
                              GPS.Editor.cursor_get_column(
                                  context.file().name()),
                              name[0],
                              "bookmarks")


GPS.parse_xml("""
  <action name="add bookmark" category="General">
     <filter id="Source editor" />
     <description>Add a bookmark for the current line</description>
     <shell lang="python" output="none">bookmarks.add_bookmark()</shell>
  </action>
  <Submenu>
     <Title>Navigate/Bookmarks</Title>
     <menu action="add bookmark">
        <Title>Add bookmark</Title>
     </menu>
  </Submenu>""")
