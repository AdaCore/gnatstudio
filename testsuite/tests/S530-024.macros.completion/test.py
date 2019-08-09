from gps_utils.internal.utils import run_test_driver, wait_tasks,\
    send_key_event, gps_assert, timeout


@run_test_driver
def driver():
    b = GPS.EditorBuffer.get(GPS.File("p.adb"))
    yield wait_tasks()
    v = b.current_view()
    v.goto(b.at(9, 8))

    GPS.execute_action("Macro Start Keyboard")
    send_key_event(ord('o'))
    yield timeout(200)
    send_key_event(ord('o'))
    yield timeout(200)
    GPS.execute_action("Macro Stop")
    v.goto(b.at(9, 1))
    GPS.execute_action("Macro Play")

    # Let the macro execute
    yield timeout(1000)
    gps_assert(b.get_chars(b.at(9, 1), b.at(9, 4)),
               "oo  ",
               "Macro playback was hindered by completion")
