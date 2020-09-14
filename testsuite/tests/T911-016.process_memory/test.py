import sys
from gs_utils.internal.utils import run_test_driver, timeout, gps_assert


@run_test_driver
def driver():
    # Launch a program that will run forever
    GPS.execute_action("Build & Run Number 1")

    # Wait until the command appears
    while True:
        tasks = filter(lambda x: x.name()=="Run Main", GPS.Task.list())
        if len(tasks) == 1:
            break
        yield timeout(100)

    # Wait a bit more
    yield timeout(100)

    # Interrupt the command
    tasks[0].interrupt()

    # Wait a bit and verify that the process interruption is showing in
    # the console
    yield timeout(100)

    if "linux" in sys.platform:
        console = GPS.Console("Run: main")
    else:
        console = GPS.Console("Run: main.exe")

    gps_assert("process interrupted" in console.get_text(), True,
               "the console didn't see the process ending: {}".format(
                    console.get_text()))

    # Now destroy the console: this shouldn't crash
    console.destroy()
    yield timeout(100)
