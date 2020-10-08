"""
This test checks the good behavior of the 'Display ancestry on navigation'
preference.
This preference is used to control whether we want to show the list of 
overridding/overridden subprograms when executing ALS navigation requests
on subprograms.
"""

from GPS import *
from gs_utils.internal.utils import *


expected_decls = ['<b>P1</b> in <b>class_definition.ads</b>',
                  '[child] <b>P1</b> in <b>class_definition.subclass.adb</b>',
                  '[child] <b>P1</b> in <b>user.adb</b>']

expected_bodies = ['[child] <b>P1</b> in <b>class_definition.subclass.adb</b>',
                   '[child] <b>P1</b> in <b>user.adb</b>']

@run_test_driver
def test_driver():

    # Execute 'goto declaration' on a dispatching call: check if
    # we display all the possible implementations

    buffer = EditorBuffer.get(File("gb.adb"))
    buffer.current_view().goto(buffer.at(11, 21))
    GPS.execute_action("goto declaration")
    yield wait_language_server('textDocument/declaration')

    menu_name = 'entity-proposals-menu'
    menu_notes_name = 'entity-proposals-menu-notes'

    yield wait_until_true(
        lambda:get_widget_by_name(menu_name) is not None)

    menu = get_widget_by_name(menu_name)
    tree = get_widgets_by_type(Gtk.TreeView, menu)[0]
    proposals = dump_tree_model(tree.get_model(), 0)
    menu_notes = get_widget_by_name(menu_notes_name)

    gps_assert (proposals, expected_decls,
                "Wrong declaration proposals for dispatching call")

    # Close the menu and set the preference to 'Never': the menu should not
    # popup this time
    menu.destroy()
    buffer.close()
    buffer = EditorBuffer.get(File("gb.adb"))
    buffer.current_view().goto(buffer.at(11, 21))

    GPS.Preference('display-ancestry-on-navigation').set('Never')
    GPS.execute_action("goto declaration")
    yield wait_language_server('textDocument/declaration')

    menu_name = 'entity-proposals-menu'
    menu = get_widget_by_name(menu_name)

    gps_assert(menu, None,
               "The menu should not popup when pref is set to 'Never'")

    current_buffer = GPS.EditorBuffer.get()
    current_loc = current_buffer.current_view().cursor()

    gps_assert(
        current_buffer.file(), GPS.File("class_definition.ads"),
        "goto declaration  did not open the right file")
    gps_assert(
        current_loc.line(), 8, "Wrong line after Go To Declaration")
    gps_assert(
        current_loc.column(), 16, "Wrong column after Go To Declaration")
    
    # Set the preference to 'Always'
    GPS.Preference('display-ancestry-on-navigation').set(
        'Always')

    current_buffer.close()
    buffer = GPS.EditorBuffer.get(GPS.File('class_definition.ads'))
    buffer.current_view().goto(buffer.at(8, 15))

    GPS.execute_action("goto body")
    yield wait_language_server('textDocument/implementation')

    menu = get_widget_by_name(menu_name)
    tree = get_widgets_by_type(Gtk.TreeView, menu)[0]
    proposals = dump_tree_model(tree.get_model(), 0)
    menu_notes = get_widget_by_name(menu_notes_name)

    gps_assert (proposals, expected_bodies,
                "Wrong declaration proposals for abstract decl")
