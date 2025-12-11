"""
This checks that opening the project file does not
causes an infinite loop, due to unsupported
textDocument/semanticTokens/full request by the ALS
server for GPR files.
"""

from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    GPS.Preference("LSP-Semantic-Highlighting").set(True)
    yield wait_tasks()
    # Open the project file
    p = GPS.EditorBuffer.get(GPS.File("default.gpr"))
    yield wait_idle()
    # Open the source code file
    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    # Check that the request is sent for the source
    # file which means that the request for the
    # project file does not block the queue
    yield wait_language_server("textDocument/semanticTokens/full")
