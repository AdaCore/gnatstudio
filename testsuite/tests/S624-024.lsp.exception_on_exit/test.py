from gps_utils.internal.utils import run_test_driver, get_widget_by_name, \
                                     dump_tree_model, gps_assert
from gps_utils import hook
from workflows.promises import timeout


@run_test_driver
def driver():
    als = GPS.LanguageServer.get_by_language_name("Ada")

    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    b.current_view().goto(b.at(4, 5))
    yield hook('language_server_response_processed')

    GPS.execute_action("Entity called by")
    yield hook('language_server_response_processed')

