from gs_utils.internal.utils import (
    run_test_driver,
    wait_tasks,
    known_tasks,
    wait_language_server,
)


@run_test_driver
def driver():
    b = GPS.EditorBuffer.get(GPS.File("cairo.c"))
    yield wait_tasks(other_than=known_tasks)
    b.insert(b.at(1, 1), "\n")
    # Give time for clangd to reparse things
    yield wait_tasks(other_than=known_tasks)
