"""
Check execution of LSP request with Python API
"""
import json
import GPS
from gps_utils.internal.utils import *

@run_test_driver
def test_driver():
    adb_file = GPS.File("main.adb")
    adb_buffer = GPS.EditorBuffer.get(adb_file)
    server = GPS.LanguageServer.get_by_language_info (adb_buffer.get_lang())

    params = {"textDocument": {"uri": adb_file.uri},
              "position": {"line": 1, "character": 11},
              "context": {"includeDeclaration": True}}

    yield timeout(1000)
    # wait till langauge server will be run

    server.request("textDocument/references", params,
                   on_result, on_error, on_reject)

def on_error(code, message, data):
    gps_assert(False, True, "error response is not expected")

def on_result(data):
    d = json.loads(data)

def on_reject():
    gps_assert(False, True, "reject is not expected")
