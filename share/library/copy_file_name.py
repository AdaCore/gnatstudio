"""
This plug-in creates a new action in GPS (which you can bind to a key
shortcut or a menu), which copies the name of the current file into
the system clipboard. You can then paste it in any application.

The current file is either the select file (if for instance the Project view
has the focus), or the name of the last editor that had the focus otherwise.
"""

import GPS
import gps_utils


@gps_utils.interactive()
def copy_file_name():
    """
    Copy the name of the current file in the clipboard, so that it can be
    pasted elsewhere.
    """

    ctxt = GPS.current_context()
    if ctxt.file():
        f = ctxt.file().path
    elif ctxt.directory():
        f = ctxt.directory()
    else:
        b = GPS.EditorBuffer.get(open=False)
        if b:
            f = b.file().path

    if not f:
        GPS.Console().write("No file found\n")
    else:
        GPS.Clipboard.copy(f)
