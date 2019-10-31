from gs_utils.internal.utils import run_test_driver, gps_assert, hook


@run_test_driver
def driver():
    # Create a new file with no name
    GPS.execute_action("new file")
    b = GPS.EditorBuffer.get()

    # Insert some ada code
    b.insert(b.at(1, 1), "package pack is\n")

    # Now save the file to its rightful name
    f = GPS.File("pack.ads")
    b.save(file=f)

    # Add some more Ada Code
    b.insert(b.at(2, 1), "   Foo : Integer := 42;\n")
    b.insert(b.at(3, 1), "   Bla : Integer := Foo;\n")
    b.insert(b.at(4, 1), "end pack;\n")

    # Do a "goto declaration" on Foo at line 3
    b.current_view().goto(b.at(3, 22))
    GPS.execute_action("goto declaration")
    yield hook('language_server_response_processed')

    # Check that "Foo" is selected at line 2
    gps_assert(b.get_cursors()[0].location().line(), 2,
               "Wrong line selected after goto declaration")
    gps_assert(b.get_chars(b.selection_start(), b.selection_end()),
               "Foo ",
               "'Foo' wasn't selected after the call to goto declaration")
