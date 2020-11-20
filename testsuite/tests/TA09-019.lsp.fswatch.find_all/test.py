# Verify that the language server correctly detects changes made on the
# filesystem

from gs_utils.internal.utils import run_test_driver, wait_language_server, \
   gps_assert, timeout, wait_tasks, known_tasks
import shutil
import os


@run_test_driver
def driver():
    yield wait_tasks()
    b = GPS.EditorBuffer.get(GPS.File("p.ads"))

    # Execute a first "find all references" for Foo and check the result
    b.current_view().goto(b.at(2, 4))
    GPS.execute_action("Find All References")
    yield wait_language_server("textDocument/references")

    gps_assert(len(GPS.Message.list(category="References for Foo (p.ads:2)")),
               2,
               "There should be two references for Foo")

    # Simulate a file change on the filesystem for a file that has never
    # been opened

    shutil.copy("q.ads.lots", "q.ads")

    # Wait a bit for the filesystem + the ALS to catch up
    yield timeout(2000)

    # Re-execute the request and verify that we have updated results
    b.current_view().goto(b.at(2, 4))
    GPS.execute_action("Find All References")
    yield wait_language_server("textDocument/references")

    gps_assert(len(GPS.Message.list(category="References for Foo (p.ads:2)")),
               5,
               "There should now be five references for Foo")

    # Now do the same as above, after deleting the file

    os.remove("q.ads")
    yield timeout(2000)
    b.current_view().goto(b.at(2, 4))
    GPS.execute_action("Find All References")
    yield wait_language_server("textDocument/references")

    gps_assert(len(GPS.Message.list(category="References for Foo (p.ads:2)")),
               1,
               "There should now be one references for Foo")
