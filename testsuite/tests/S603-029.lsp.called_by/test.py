from gs_utils.internal.utils import (
    run_test_driver,
    get_widget_by_name,
    dump_tree_model,
    gps_assert,
)
from gs_utils import hook
from pygps import double_click_events
from pygps.tree import click_in_tree
from workflows.promises import timeout, wait_tasks, known_tasks, wait_idle


@run_test_driver
def driver():
    als = GPS.LanguageServer.get_by_language_name("Ada")

    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    b.current_view().goto(b.at(4, 5))
    yield wait_idle()

    GPS.execute_action("Entity called by")
    yield hook("language_server_response_processed")
    yield timeout(500)

    call_tree = get_widget_by_name("Call Graph Tree")
    selection = call_tree.get_selection()
    selection.unselect_all()
    model = call_tree.get_model()
    selection.select_iter(model.iter_nth_child(model.get_iter_first(), 0))

    GPS.execute_action("calltree expand selected")
    yield hook("language_server_response_processed")
    yield timeout(500)

    expected = [
        "Foo is called by ",
        [
            "Foo",
            ["Foo", ["computing..."], "Main", ["computing..."]],
            "Main",
            ["computing..."],
        ],
    ]

    gps_assert(
        expected,
        dump_tree_model(model, 0),
        "The model didn't contain the expected text",
    )

    # Now verify that double-clicking on the row that lists 'Main'
    # correctly open its editor.

    GPS.execute_action("close all editors")
    yield wait_tasks(other_than=known_tasks)

    click_in_tree(
        call_tree, path=Gtk.TreePath("0:0:1"), button=1, events=double_click_events
    )
    yield wait_idle()

    buffer = GPS.EditorBuffer.get()
    gps_assert(
        buffer.file(),
        GPS.File("main.adb"),
        "double-clicking on a Call Trees row should open an "
        + "editor for the clicked entity",
    )
