"""
Check the correct creation of secondaries messages for a stack of messages
at the same location.
"""

from GPS import *
from gs_utils.internal.utils import *

expected = ['Builder results (2 items in 1 file)',
            ['foo.adb (2 items)',
             ['<b>7:15</b>      no candidate interpretations match the' +
              ' actuals:',
              ['          missing argument for parameter &quot;Item&quot;' +
               ' in call to &quot;Put&quot; declared at <span' +
               ' color="#729FCF"><u>a-textio.ads:459</u></span>',
               '          missing argument for parameter &quot;Item&quot;' +
               ' in call to &quot;Put&quot; declared at <span' +
               ' color="#729FCF"><u>a-textio.ads:386</u></span>'],
              '<b>7:21</b>      expected type &quot;Standard.String&quot;',
              ['          found private type &quot;' +
               'Ada.Strings.Unbounded.Unbounded_String&quot;',
               '            ==&gt; in call to &quot;Put&quot; at <span' +
               ' color="#729FCF"><u>a-textio.ads:465</u></span>',
               '            ==&gt; in call to &quot;Put&quot; at <span' +
               ' color="#729FCF"><u>a-textio.ads:392</u></span>']]]]


@run_test_driver
def run_test():
    with open("message.txt", "r") as f:
        GPS.Locations.parse(f.read(), "Builder results")
    gps_assert(dump_locations_tree(),
               expected,
               "Issue during creation of secondaries messages")
