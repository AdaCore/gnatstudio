from gps_utils.internal.utils import run_test_driver, get_widget_by_name, \
                                     dump_tree_model, gps_assert
from gps_utils import hook
from workflows.promises import timeout


@run_test_driver
def driver():
    yield timeout(1000)
    als = GPS.LanguageServer.get_by_language_name("Ada")

    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    b.current_view().goto(b.at(4, 5))
    yield hook('language_server_response_processed')

    GPS.execute_action("Entity called by")
    yield hook('language_server_response_processed')
    yield timeout(1000)

    call_tree = get_widget_by_name("Call Graph Tree")
    selection = call_tree.get_selection()
    selection.unselect_all()
    model = call_tree.get_model()
    selection.select_iter(model.iter_nth_child(model.get_iter_first(), 0))

    GPS.execute_action("calltree expand selected")
    yield hook('language_server_response_processed')

    expected = ['Foo is called by ',
                ['Foo', ['Foo', ['computing...'],
                         'Main', ['computing...']],
                 'Main', ['computing...']]]

    yield timeout(1000)
    gps_assert(expected, dump_tree_model(model, 0),
               "The model didn't contain the expected text")
