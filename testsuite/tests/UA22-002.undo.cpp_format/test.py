"""
One Undo should revert One workspaceEdit when it affects only one file (this
is not a global command)
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    GPS.Preference("Editor-Range-Formatter-ada").set("LSP")
    buf = GPS.EditorBuffer.get(GPS.File("main.cpp"))
    GPS.execute_action("format selection")
    yield wait_language_server("textDocument/rangeFormatting", "C++")

    GPS.execute_action("undo")
    GPS.execute_action("redo")
    try:
        GPS.execute_action("redo")
        simple_error("Issue in the redo queue")
    except GPS.Exception:
        # An error is expected
        pass
    GPS.execute_action("undo")
    try:
        GPS.execute_action("undo")
        simple_error("Issue in the undo queue")
    except GPS.Exception:
        # An error is expected
        pass
