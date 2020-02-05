"""Check that 'is called by' works when queried from the runtime"""

import os
from gs_utils.internal.utils import run_test_driver, wait_tasks, known_tasks,\
   gps_assert, get_widget_by_name, dump_tree_model
from gs_utils import hook
from pygps.tree import click_in_tree
from workflows.promises import timeout


@run_test_driver
def driver():
    # Open bla.adb, goto declaration of "Create"
    b = GPS.EditorBuffer.get(GPS.File("bla.adb"))
    b.current_view().goto(b.at(6, 16))
    GPS.execute_action("goto declaration")
    yield wait_tasks(other_than=known_tasks)

    # Sanity check that we're on the runtime file
    b2 = GPS.EditorBuffer.get()
    gps_assert(os.path.basename(b2.file().name()),
               "a-textio.ads",
               "goto declaration didn't work")

    # Run "is called by"
    GPS.execute_action("Entity called by")
    yield hook('language_server_response_processed')
    yield timeout(1000)

    # Verify that the call tree does show the call location in "Bla"
    call_tree = get_widget_by_name("Call Graph Tree")
    model = call_tree.get_model()
    gps_assert(dump_tree_model(model, 0),
               ['Create is called by ', ['Bla', ['computing...']]],
               "The model didn't contain the expected result")
