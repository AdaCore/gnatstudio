# Check where a code action starts and finishes

from gs_utils.internal.utils import run_test_driver, wait_language_server, \
    gps_assert, get_widget_by_name, timeout


@run_test_driver
def driver():

    def is_valid(expected, msg):
        """
        `expected` : bool indicating if we should have a code action
        `msg` : string for error message
        """
        m = GPS.Message.list()
        if expected:
            gps_assert(len(m), 1, "error at " + msg)
            gps_assert(m[0].get_category(), "_internal_code_actions",
                       "wrong category")
        else:
            gps_assert(len(m), 0, "error at " + msg)

    # Open an editor and go to a line where there's a code action
    b = GPS.EditorBuffer.get(GPS.File("foo.adb"))
    v = b.current_view()

    # go to the start of Hello and wait for the codeaction
    v.goto(b.at(2, 28))
    yield wait_language_server("textDocument/codeAction")
    is_valid(True, "start of Hello")

    # go to the last index inside hello
    v.goto(b.at(2, 32))
    yield wait_language_server("textDocument/codeAction")
    is_valid(True, "end of Hello")

    # go to the first index before hello
    v.goto(b.at(2, 27))
    yield wait_language_server("textDocument/codeAction")
    is_valid(False, "before Hello")

    # go to the last index inside hello
    v.goto(b.at(2, 33))
    yield wait_language_server("textDocument/codeAction")
    is_valid(False, "after Hello")
