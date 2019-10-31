"""
The "Search" action behavior changed depending on the focus.
Test it in all the configuration: docked/float + with/without incremental
"""


import GPS
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs

INCREMENTAL_PREF = "Search-Incremental"
ACTION = "Search"
OUT = 1000


def reset_search_context():
    s = GPS.Search.lookup(GPS.Search.FILE_NAMES)
    s.set_pattern("Main", flags=GPS.Search.FUZZY)
    s.get()


def verify_loc(view, line, column, msg):
    gps_assert(view.cursor().line(), line, "Line issue: " + msg)
    gps_assert(view.cursor().column(), column, "Column issue: " + msg)


def play_scenario(msg, is_docked):
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    view = buf.current_view()
    reset_search_context()
    # Be sure that the file has the focus
    editor = GPS.MDI.get("main.adb")
    editor.raise_window()

    # Start a search
    buf.select(buf.at(2, 4), buf.at(2, 7))
    verify_loc(view, 2, 7, "Wrong location via EditorBuffer for " + msg)
    gps_assert(GPS.MDI.current().name(short=True),
               "main.adb",
               "The editor should have the focus")
    GPS.execute_action(ACTION)
    yield wait_tasks(other_than=known_tasks)
    yield timeout(OUT)
    search = GPS.MDI.get("Search")
    gps_assert(search is not None,
               True,
               "The Search view should be opened for " + msg)
    verify_loc(view, 2, 4,
               "Wrong location at the start of the search for " + msg)
    gps_assert(GPS.MDI.current().name(short=True),
               "Search",
               "The Search view should have the focus")

    # The focus is in the Search view:
    # the "Search" action should find the next match
    GPS.execute_action(ACTION)
    yield wait_tasks(other_than=known_tasks)
    yield timeout(OUT)
    verify_loc(view, 2, 4,
               "Wrong location after the second search for " + msg)
    # Give the focus to the editor and move the cursor
    editor = GPS.MDI.get("main.adb")
    editor.raise_window()
    view.goto(buf.at(1, 11))
    yield wait_tasks(other_than=known_tasks)
    yield timeout(OUT)

    # The cursor has been moved by the user => The "Search" action should not
    # change the cursor location, give the focus to the Search view
    # and preselect the "Find" entry
    GPS.execute_action(ACTION)
    yield wait_tasks(other_than=known_tasks)
    yield timeout(OUT)
    verify_loc(view, 1, 11,
               "Search failed when the cursor is in the editor for " + msg)
    gps_assert(GPS.MDI.current().name(),
               "Search",
               "The search view should have the focus for " + msg)
    if is_docked:
        combos = get_widgets_by_type(Gtk.ComboBox, search.pywidget())
    else:
        combos = get_widgets_by_type(Gtk.ComboBox,
                                     get_window_by_prefix('GPS - Search -'))
    entry = combos[0].get_child()
    gps_assert(entry.get_selection_bounds(),
               (0, 3),
               "The entry text should be preselectioned for " + msg)

    # Close the buffer and the view in preparation for the next run
    buf.close(force=True)
    GPS.MDI.get("Search").close(force=True)


@run_test_driver
def test_driver():
    GPS.Preference(INCREMENTAL_PREF).set(False)
    yield play_scenario("Float + without incremental", False)
    GPS.Preference(INCREMENTAL_PREF).set(True)
    yield play_scenario("Float + with incremental", False)
    GPS.execute_action(ACTION)
    view = GPS.MDI.get("Search")
    view.raise_window()
    GPS.execute_action("Unfloat view")
    yield play_scenario("Docked + with incremental", True)
    GPS.Preference(INCREMENTAL_PREF).set(False)
    GPS.execute_action(ACTION)
    view = GPS.MDI.get("Search")
    view.raise_window()
    GPS.execute_action("Unfloat view")
    yield play_scenario("Docked + without incremental", True)
