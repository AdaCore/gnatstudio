"""
This file provides read-only protection for areas surrounded by markers;

--  begin read only
--  end read only
"""

###########################################################################
# No user customization below this line
###########################################################################

import GPS
from gs_utils import hook, interactive

read_only_pref = GPS.Preference(
    "Editor/Fonts & Colors:General/read_only_color").create_with_priority(
        "Read-only code",
        "color",
        -2,
        "",
        "#e0e0e0")

overlay_name = "read_only_region"
read_only_files = []


@interactive(category="Editor", name="Toggle read-only regions in an editor")
def toggle_read_only():
    global read_only_files
    buffer = GPS.EditorBuffer.get()

    if buffer:
        file = buffer.file()

        if file in read_only_files:
            read_only_files.remove(file)
            read_only_overlay = buffer.create_overlay(overlay_name)
            buffer.remove_overlay(read_only_overlay)
        else:
            mark_read_only_areas(buffer)


@hook('file_edited')
def __on_file_edited(file):
    editor = GPS.EditorBuffer.get(file, open=False)

    # Mark the read-only regions only if the file being edited has
    # an editor opened for it.
    if editor:
        mark_read_only_areas(editor)


@hook('preferences_changed')
def __on_pref_changed():
    """  Update the color of read-only code areas. """
    for file in read_only_files:
        buffer = GPS.EditorBuffer.get(file, force=False, open=False)

        if buffer:
            mark_read_only_areas(buffer)


def mark_read_only_areas(buffer):
    global read_only_files

    read_only_overlay = None
    loc = buffer.at(1, 1)
    file = buffer.file()

    if file in read_only_files:
        read_only_files.remove(file)

    # Iterate over read-only areas
    while loc:
        found = loc.search("--  begin read only", dialog_on_failure=False)

        if found:
            from_line, last = found
            found = last.search("--  end read only", dialog_on_failure=False)

            if found:
                to_line, loc = found
            else:
                loc = None

        else:
            loc = None

        # if area found
        if loc:
            from_line = from_line.beginning_of_line()
            to_line = to_line.end_of_line()

            # if overlay hasn't exist yet, create one
            if not read_only_overlay:
                read_only_overlay = buffer.create_overlay(overlay_name)
                color = read_only_pref.get()
                read_only_overlay.set_property("paragraph-background", color)
                read_only_overlay.set_property("editable", False)

                # Append it to the global list of read-only code locations
                read_only_files.append(file)

            buffer.apply_overlay(read_only_overlay, from_line, to_line)
    # No more read-only areas
