"""This plug-in adds a menu File->Messages->Export Locations to Editor
   which opens an editor with the contents of the Locations view.
   It also add contextual menu to clear items for current file from
   location view.
"""

import GPS
import gps_utils
import os.path


def message_compare(a, b):
    """ Comparison function between two messages: compare messages based on
        line, then column, then text.
    """
    if a.get_line() < b.get_line():
        return -1
    if a.get_column() < b.get_column():
        return -1
    if a.get_text() < b.get_text():
        return -1
    return 1


def in_locations_filter(context):
    return context.module_name == "Location_View_Record"


@gps_utils.interactive(
    name="export locations to editor",
    contextual="Export messages to editor",
    filter=in_locations_filter,
    after="Change Directory...")
def export_locations_to_editor():
    """
    Export all messages listed in the Locations view to an editor.
    """

    categories = {}
    files = {}

    # Get all messages

    msgs = GPS.Message.list()

    # Filter them and organize them by category and file
    for m in msgs:
        if m.get_flags() & 2 == 0:
            file = m.get_file()
            category = m.get_category()

            if category in categories:
                if file in categories[category]:
                    categories[category][file] += [m]
                else:
                    categories[category][file] = [m]
            else:
                categories[category] = {file: [m]}

    if not categories:
        GPS.MDI.dialog("The Locations view is empty.")
        return

    # Construct a string that we will write in the editor

    text = ""

    categories_list = [c for c in categories]
    categories_list.sort()

    for c in categories_list:
        text += c + "\n"

        files_list = [f for f in categories[c]]
        files_list.sort()

        for f in files_list:
            text += "    " + f.name() + "\n"
            messages = categories[c][f]
            messages.sort(message_compare)

            for m in messages:
                text += "        %s:%s %s\n" % (
                    m.get_line(),
                    m.get_column(),
                    m.get_text())

        text += "\n"

    # Open an editor

    GPS.execute_action("/File/New")
    buf = GPS.EditorBuffer.get()

    # Write the contents
    buf.insert(buf.at(1, 1), text)


def on_filter(context):
    if context.file():
        # Return True if there are any messages in the file context
        # which have the flag '2' set to 0, meaning that they show up in
        # the Locations view.
        for m in GPS.Message.list(file=context.file()):
            if not m.get_flags() & 2:
                return True

        return False


@gps_utils.interactive(
    category='Locations', filter=on_filter,
    name='Clear locations for file',
    contextual=lambda ctx: 'Clear locations for <b>%s</b>' % (
         os.path.basename(ctx.file().name())), )
def on_contextual():
    context = GPS.current_context()
    list = GPS.Message.list(file=context.file())
    for m in list:
        m.remove()
