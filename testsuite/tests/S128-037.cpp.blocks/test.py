from gs_utils.internal.utils import (run_test_driver, gps_assert, hook,
                                     wait_tasks,
                                     get_widget_by_name, timeout)


@run_test_driver
def driver():
    yield wait_tasks()

    b = GPS.EditorBuffer.get(GPS.File("t.cpp"))

    # Wait until the semantic tree is available, then move the cursor
    yield hook('semantic_tree_updated')

    # Move the cursor line by line and verify that GPS doesn't freeze
    for line in range(20, 40):
        b.current_view().goto(b.at(line, 1))
        yield timeout(500)
