"""This plug-in adds a menu File->Messages->Export Locations to editor
   which opens an editor with the contents of the Locations view.
"""


import GPS

def message_compare (a, b):
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

def remove_markup(text):
    """ Remove pango markup from text """

    remove = False
    result = ""
    for c in text:
        if c == '<':
            remove=True
        elif c == '>':
            remove=False
        else:
            if not remove:
                result += c

    return result

def export_locations_to_editor (menu):
    """ Export all messages listed in the Locations view to an editor.
    """

    categories = {}
    files      = {}

    # Get all messages

    msgs = GPS.Message.list()

    # Filter them and organize them by category and file
    for m in msgs:
        if m.get_flags() & 2 == 0:
            file     = m.get_file()
            category = m.get_category()

            if category in categories:
                if file in categories[category]:
                    categories[category][file]+=[m]
                else:
                    categories[category][file]=[m]
            else:
                categories[category]={file:[m]}

    if not categories:
        GPS.MDI.dialog ("The Locations view is empty.")
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
             messages.sort (message_compare)

             for m in messages:
                 text += "        %s:%s %s\n" % (
                    m.get_line(),
                    m.get_column(),
                    remove_markup(m.get_text()))

        text += "\n"

    # Open an editor

    GPS.execute_action ("/File/new")
    buf = GPS.EditorBuffer.get()

    # Write the contents
    buf.insert (GPS.EditorLocation (buf, 1, 1), text)


GPS.Menu.create ("/File/Messages/Export Locations to editor", export_locations_to_editor)

