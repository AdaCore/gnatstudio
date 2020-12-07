# This tests that the preference for "conditional continuation lines" is
# not (wrongly) taken into account when indenting expression functions

from gs_utils.internal.utils import run_test_driver, gps_assert

expected = """procedure t is
   function F (A : Boolean; B : Boolean) return Boolean is (True);

   function F2 return Boolean is
       (F (A => True,
           B => False));"""


@run_test_driver
def driver():

    # Set the conditional continuation pref to a ridiculous amount
    GPS.Preference("Ada-Conditional-Level").set(120)
    b = GPS.EditorBuffer.get(GPS.File("t.adb"))

    # Autoindent the second line in the aggregate
    b.current_view().goto(b.at(6, 6))
    GPS.execute_action("Autoindent selection")

    # Verify that the proper indentation is produced
    gps_assert(b.get_chars(b.at(1, 1), b.at(6, 24)),
               expected,
               "Wrong autoindent for aggregate in expression function")
