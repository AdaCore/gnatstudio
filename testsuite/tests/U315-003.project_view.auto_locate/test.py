"""
Check if GS "Auto-Locate" works for filtered files.
"""

from gs_utils.internal.utils import *
from workflows.promises import known_tasks


@run_test_driver
def driver():
    b = GPS.EditorBuffer.get(GPS.File("hello.adb"))
    v = b.current_view()

    prj_view = Project_View()
    yield prj_view.open_and_yield()

    explorer = prj_view.dialog
    explorer.expand_all()

    filt = get_widget_by_name("Project Explorer Filter")
    filt.set_text("hello")
    yield hook("filter_view_changed")

    b = GPS.EditorBuffer.get(GPS.File("aaa.ads"))
    GPS.execute_action("Locate file in explorer (no focus)")
    dump = dump_tree_model(explorer.get_model(), 1)
    gps_assert(filt.get_text(), "", "Filter hasn't cleared")
    gps_assert(dump,
               ['Default', ['.', ['aaa.ads', 'hello.adb'], '.']],
               "Project view content wrong after locate")
