"""
Edit a project file with a Coverage package, only the unknown attribute should
be removed.
"""

from GPS import *
from gs_utils.internal.utils import *


@run_test_driver
def run_test():
    root = GPS.Project.root()

    def _assert_val(suffix_msg, not_edited=False):
        # Check the value of known attribute
        gps_assert(root.get_attribute_as_list("Units", "Coverage"),
                   ["AAA"],
                   "Wrong value for Units" + suffix_msg)
        gps_assert(root.get_attribute_as_string("EXCLUDED_UNITS_LIST",
                                                "Coverage"),
                   "BBB",
                   "Wrong value for Excluded_Units_List" + suffix_msg)
        gps_assert(root.get_attribute_as_string("Unknown_Attr", "Coverage"),
                   "",
                   "Wrong value for Unknown_Attr" + suffix_msg)

        # Check value by indexed for Switches
        gps_assert(root.get_attribute_as_list("SWITCHES", "Coverage", "*"),
                   ["DDD"],
                   "Wrong value for Units" + suffix_msg)
        gps_assert(root.get_attribute_as_list("SWITCHES",
                                              "Coverage",
                                              "instrument"),
                   ["EEE"],
                   "Wrong value for Excluded_Units_List" + suffix_msg)

        # Check the Unknown attribute presence in the file
        buf = GPS.EditorBuffer.get(root.file())
        text = buf.get_chars()
        gps_assert('for Unknown_Attr use "CCC";' in text,
                   not_edited,
                   "Failed checking Unknown_Attr in text " + suffix_msg)
        buf.close()

        gps_assert(root.get_attribute_as_string("Casing", "NAMING"),
                   "lowercase" if not_edited else "UPPERCASE",
                   "Wrong value for Naming'Casing" + suffix_msg)

    _assert_val(" after loading the project", True)

    dialog = Project_Properties_Editor()
    yield dialog.open_and_yield()

    page = dialog.get_page('Sources/Naming/Ada')
    entry = get_widgets_by_type(Gtk.ComboBoxText, page)[1]
    entry.set_active(0)
    yield dialog.save()

    _assert_val(" after editing the project with the wizard", False)
