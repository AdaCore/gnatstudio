from gs_utils.internal.utils import (
    run_test_driver,
    wait_language_server,
    gps_assert,
    timeout,
)

#  Test proper unfolding of a block when an edit
#  happens on this block.

expected = """procedure Test_Fold is

   procedure P1;

   procedure P2;

   procedure P1 is

   procedure P2 is
   begin
      null;
   end P2;
   begin
      null;
   end P1;

begin
   null;
end Test_Fold;
"""


@run_test_driver
def driver():
    buf = GPS.EditorBuffer.get(GPS.File("test_fold.adb"))
    yield wait_language_server("textDocument/foldingRange")
    buf.select(buf.at(10, 11), buf.at(15, 11))
    GPS.execute_action("Cut to Clipboard")
    yield wait_language_server("textDocument/foldingRange")
    buf.at(7, 1).block_fold()
    buf.current_view().goto(buf.at(7, 19))
    GPS.execute_action("Paste from Clipboard")
    yield wait_language_server("textDocument/foldingRange")

    # Verify that the contents is what we logically expect
    gps_assert(
        buf.get_chars(), expected, "Buffer corruption when inserting after folded block"
    )

    # Verify that the logical contents correspond to the contents
    # on screen!
    g = buf.gtk_text_buffer()
    gps_assert(
        buf.get_chars(),
        g.get_text(g.get_start_iter(), g.get_end_iter(), False),
        "Corruption between the visible text and the logical text",
    )

    # Last layer of safety: verify that saving + undo works
    buf.undo()
    buf.undo()
    buf.save(file=GPS.File("out.txt"))
