"""
Check execution of LSP request with Python API
"""
import json
import GPS
from gs_utils.internal.utils import *
from workflows.promises import known_tasks


@run_test_driver
def test_driver():
    adb_file = GPS.File("main.adb")
    adb_buffer = GPS.EditorBuffer.get(adb_file)
    server = GPS.LanguageServer.get_by_language_info (adb_buffer.get_lang())

    params = {"textDocument": {"uri": adb_file.uri},
              "position": {"line": 1, "character": 11},
              "context": {"includeDeclaration": True}}

    # wait LSP responses has been processed to have folding information
    if GPS.LanguageServer.is_enabled_for_language_name("Ada"):
        yield wait_tasks(other_than=known_tasks)

    server.request("textDocument/references", params,
                   on_result, on_error, on_reject)
    yield hook('language_server_response_processed')

def on_error(code, message, data):
    gps_assert(False, True, "error response is not expected")

def on_result(data):
    d = json.loads(data)

def on_reject():
    gps_assert(False, True, "reject is not expected")
