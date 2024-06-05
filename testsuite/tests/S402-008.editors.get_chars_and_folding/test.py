"""
Test get_chars(include_hidden_chars=False).
"""

import GPS
from gs_utils.internal.utils import *
from workflows.promises import known_tasks

FILE = "main.adb"
EXPECTED = """with Ada.Text_IO;

procedure Main is
begin
   if 1 + 1 = 2 then
end Main;
"""


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File(FILE))
    initial = buf.get_chars(include_hidden_chars=True)

    # wait LSP responses has been processed
    if GPS.LanguageServer.is_enabled_for_language_name("Ada"):
        yield wait_tasks(other_than=known_tasks)

    yield timeout(2000)
    buf.blocks_fold()

    gps_assert(
        buf.get_chars(include_hidden_chars=False),
        EXPECTED,
        "Wrong string when ignoring hidden chars",
    )
    gps_assert(
        buf.get_chars(include_hidden_chars=True),
        initial,
        "Wrong string when ignoring hidden chars",
    )
