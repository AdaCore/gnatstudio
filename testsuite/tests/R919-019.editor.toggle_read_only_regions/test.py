"""
Check toggle read only region action.
"""
from gs_utils.internal.utils import *
from workflows import run_as_workflow


@run_as_workflow
def test_ro_region(ro_region, allow_edit):
    """
    Try to modify an editor at given location (ro_region)
    """
    editor = ro_region.buffer()
    editor.current_view().goto(ro_region)
    send_key_event(66)  ##  'B'
    yield wait_idle()
    line = ro_region.get_word()[0]
    gps_assert(line != "begin", allow_edit)

    if allow_edit:
        editor.undo()


@run_test_driver
def driver():
    editor = GPS.EditorBuffer.get(GPS.File("main.adb"))
    ro_region = editor.at(3,1)
    # Check that no editing allowed
    yield test_ro_region(ro_region, allow_edit=False)
    GPS.execute_action("Toggle read-only regions in an editor")
    # Check that an editing allowed now
    yield test_ro_region(ro_region, allow_edit=True)
    GPS.execute_action("Toggle read-only regions in an editor")
    # Check that no editing allowed again
    yield test_ro_region(ro_region, allow_edit=False)
