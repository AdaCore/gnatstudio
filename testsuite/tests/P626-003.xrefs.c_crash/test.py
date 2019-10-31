from gs_utils.internal.utils import run_test_driver, wait_tasks, known_tasks


@run_test_driver
def driver():
    b = GPS.EditorBuffer.get(GPS.File("cairo.c"))
    b.insert(b.at(1, 1), "\n")
    # Give time for libclang to reparse things
    yield wait_tasks(other_than=known_tasks)
