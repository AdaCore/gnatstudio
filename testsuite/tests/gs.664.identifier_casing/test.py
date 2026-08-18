"""
Test "Identifier Casing" preference and ALS compatibility.
"""

import GPS
from gs_utils.internal.utils import (
    run_test_driver,
    wait_idle,
    gps_assert,
    wait_language_server,
)

expected_mixed = """   function Hello_World return Boolean
   is (Bye_Bye (X_A => True, X_B => False));

"""

expected_lower = """   function hello_world return boolean
   is (bye_bye (x_a => true, x_b => false));

"""

expected_upper = """   function HELLO_WORLD return BOOLEAN
   is (BYE_BYE (X_A => TRUE, X_B => FALSE));

"""

expected_keep = """   function Hello_World return Boolean
   is (BYE_bye (X_A => True, x_b => False));

"""


def testcase(buf, pref_val, expected):
    GPS.Preference("Ada-Ident-Casing").set(pref_val)

    buf.current_view().goto(buf.at(5, 16))
    GPS.execute_action("format selection")
    yield wait_language_server("textDocument/rangeFormatting")
    yield wait_idle()

    gps_assert(
        buf.get_chars(buf.at(4, 1), buf.at(6, 1)),
        expected,
        "Wrong formatting with %s" % pref_val,
    )


@run_test_driver
def driver():
    GPS.Preference("Editor-Range-Formatter-ada").set("LSP")
    b = GPS.EditorBuffer.get(GPS.File("t.adb"))
    yield wait_idle()

    yield testcase(b, "Unchanged", expected_keep)
    yield testcase(b, "Upper", expected_upper)
    yield testcase(b, "Lower", expected_lower)
    yield testcase(b, "Mixed", expected_mixed)
