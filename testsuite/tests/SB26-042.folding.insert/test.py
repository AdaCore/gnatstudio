"""Verify that inserting text after a folded block doesn't unfold it."""

from gs_utils.internal.utils import run_test_driver, gps_assert, wait_tasks
from workflows.promises import known_tasks

expected_1 = """procedure hello is
   procedure fold_me is
begin
   -- insert here:
   null;
end;
"""

expected_2 = """procedure hello is
   procedure fold_me is
begin
   -- insert here:a
   null;
end;
"""


@run_test_driver
def driver():
    b = GPS.EditorBuffer.get(GPS.File("hello.adb"))
    v = b.current_view()

    # wait LSP responses has been processed
    if GPS.LanguageServer.is_enabled_for_language_name("Ada"):
        yield wait_tasks(other_than=known_tasks)

    b.blocks_fold()
    gps_assert(b.get_chars(include_hidden_chars=False),
               expected_1,
               "block folding didn't happen")

    b.insert(b.at(7, 19), "a")

    gps_assert(b.get_chars(include_hidden_chars=False),
               expected_2,
               "contents not right after inserting character")
