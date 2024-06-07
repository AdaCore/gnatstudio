import sys
from gs_utils.internal.utils import (
    run_test_driver,
    timeout,
    gps_assert,
    wait_until_true,
)


@run_test_driver
def driver():
    # Launch a program that will run forever
    GPS.execute_action("Build & Run Number 1")

    # Wait until the command appears
    while True:
        tasks = [x for x in GPS.Task.list() if x.name() == "Run Main"]
        if len(tasks) == 1:
            break
        yield timeout(100)

    if "linux" in sys.platform:
        console = GPS.Console("Run: main")
    else:
        console = GPS.Console("Run: main.exe")

    # Wait a bit more
    yield wait_until_true(lambda: console.get_text() != "")

    # Interrupt the command
    tasks[0].interrupt()

    yield wait_until_true(lambda: "process interrupted" in console.get_text())

    gps_assert(
        "process interrupted" in console.get_text(),
        True,
        "the console didn't see the process ending: {}".format(console.get_text()),
    )

    # Now destroy the console: this shouldn't crash
    console.destroy()
    yield timeout(100)
