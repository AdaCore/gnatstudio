from gs_utils.internal.utils import run_test_driver, get_widget_by_name, \
                                     dump_tree_model, gps_assert
from gs_utils import hook
from pygps import double_click_events
from pygps.tree import click_in_tree
from workflows.promises import timeout, wait_tasks, known_tasks, wait_idle


@run_test_driver
def driver():
    yield timeout(1000)
    als = GPS.LanguageServer.get_by_language_name("Ada")

    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    b.current_view().goto(b.at(4, 5))
    yield hook('language_server_response_processed')

    GPS.execute_action("Entity calls")
    yield hook('language_server_response_processed')
    yield timeout(1000)

    call_tree = get_widget_by_name("Call Graph Tree")
    selection = call_tree.get_selection()
    selection.unselect_all()
    model = call_tree.get_model()
    selection.select_iter(model.iter_nth_child(model.get_iter_first(), 1))

    GPS.execute_action("calltree expand selected")
    yield hook('language_server_response_processed')

    expected = ['Foo calls ',
                ['Bla', ['computing...'],
                 'Foo', ['Bla',
                         ['computing...'],
                         'Foo',
                         ['computing...'],
                         'Put_Line',
                         ['computing...']],
                 'Put_Line', ['computing...']]]

    yield timeout(1000)
    gps_assert(expected, dump_tree_model(model, 0),
               "The model didn't contain the expected text")

    # Now verify that double-clicking on the row that lists 'Main'
    # correctly open its editor and selects it.

    GPS.execute_action("close all editors")
    yield wait_tasks(other_than=known_tasks)

    click_in_tree(call_tree, path=Gtk.TreePath("0:1:1"),
                  button=1, events=double_click_events)
    yield wait_idle()

    buffer = GPS.EditorBuffer.get()
    gps_assert(buffer.file(), GPS.File("p.ads"),
               "double-clicking on a Call Trees row should open an "
               + "editor for the clicked entity")
    gps_assert((buffer.selection_start(), buffer.selection_end()),
               (buffer.at(4,14), buffer.at(4,17)),
               "Foo should be selected in p.ads after double-clicking "
               + "on its row in the Call Trees")
