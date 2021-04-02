"""
Verify that clipboard actions work fine with folded blocks
In particular,copying should copy the folded contents when
the selection includes the folded block.
"""

from gs_utils.internal.utils import *
from workflows.promises import known_tasks


EXPECTED = """   procedure fold_me is
   begin
      null;
   end;"""


@run_test_driver
def driver():
    b = GPS.EditorBuffer.get(GPS.File("hello.adb"))
    v = b.current_view()

    # wait LSP responses has been processed
    if GPS.LanguageServer.is_enabled_for_language_name("Ada"):
        yield wait_tasks(other_than=known_tasks)

    yield timeout(1000)
    b.blocks_fold()

    v.goto(b.at(2, 1))
    v.goto(b.at(6, 1), extend_selection=True)

    GPS.execute_action("copy to clipboard")

    gps_assert(
        GPS.Clipboard.contents()[GPS.Clipboard.current()].strip(),
        EXPECTED.strip(),
        "Wrong clipboard after copying folded block")
