"""
This plug-in creates a new actions (copy_file_name, copy_base_file_name) in
GPS (which you can bind to a key shortcut or a menu), which copies the full or
base name of the current file into the system clipboard. You can then paste it
in any application.

The current file is either the select file (if for instance the Project view
has the focus), or the name of the last editor that had the focus otherwise.
"""

import GPS
import gs_utils


def get_filename_from_context(ctxt, base_name=False):
    """
    Return the filename associated to the given context.
    The full filename or only the base name will be returned depending
    on ``base_name``.
    """
    filename = None
    f = ctxt.file()
    b = GPS.EditorBuffer.get(f, open=False) if f else None

    if b:
        filename = f.base_name() if base_name else f.path
    elif f:
        filename = f.base_name() if base_name else f.path
    elif ctxt.directory():
        if not base_name:
            filename = ctxt.directory()

    return filename


def on_copy_file_name_label(ctxt):
    filename = get_filename_from_context(ctxt, base_name=False)

    if not filename:
        return ""
    else:
        return "Copy full name to clipboard"


def on_copy_base_name_label(ctxt):
    filename = get_filename_from_context(ctxt, base_name=True)

    if not filename:
        return ""
    else:
        return "Copy base name to clipboard"


@gs_utils.interactive(contextual=on_copy_file_name_label,
                      contextual_group=GPS.Contextual.Group.EXTRA_INFORMATION)
def copy_file_name():
    """
    Copy the full name of the current file in the clipboard, so that it can be
    pasted elsewhere.
    """

    ctxt = GPS.current_context()
    filename = get_filename_from_context(ctxt, base_name=False)

    if not filename:
        GPS.Console().write("No file found\n")
    else:
        GPS.Clipboard.copy(filename)


@gs_utils.interactive(contextual=on_copy_base_name_label,
                      contextual_group=GPS.Contextual.Group.EXTRA_INFORMATION)
def copy_base_file_name():
    """
    Copy the base name of the current file in the clipboard, so that it can be
    pasted elsewhere.
    """

    ctxt = GPS.current_context()
    filename = get_filename_from_context(ctxt, base_name=True)

    if not filename:
        GPS.Console().write("No file found\n")
    else:
        GPS.Clipboard.copy(filename)
