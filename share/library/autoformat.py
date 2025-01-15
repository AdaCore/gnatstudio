"""This plugin will automatically reformat a source file each time it is
saved on disk. See also autognatpp.py for another version using the external
gnatpp pretty-printer instead.
"""

import GPS

# Actions to be performed each time a file is saved


def on_file_saved(hook, file):
    if file.language() == "python":
        # Deactivate on Python files: the formatting action
        # indents the entire selection - this is intended for user
        # selection, but it is not suitable to do this automatically.
        return

    buf = GPS.EditorBuffer.get(file)
    # Save the cursor location
    view = buf.current_view()
    if view.cursor().line() == 0:
        # This is the case when the cursor is inside a special line
        mark = None
    else:
        mark = view.cursor().create_mark()

    # Reformat the buffer
    buf.indent()

    if mark:
        # Restore the cursor location
        view.goto(mark.location())
        view.center(view.cursor())
        mark.delete()


# Register the callback on the "before_file_saved" hook
GPS.Hook("before_file_saved").add(on_file_saved)
