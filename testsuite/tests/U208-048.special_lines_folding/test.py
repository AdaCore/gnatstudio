"""
 This tests that folding works well when special lines present
"""

import GPS
from gs_utils.internal.utils import run_test_driver, gps_assert, \
    wait_tasks, wait_idle, wait_language_server, timeout, \
    wait_until_true

exp1 = """procedure Main is

   Even_Number_Count, Odd_Number_Count : Natural := Natural'First;

begin

   for I in 1 .. 100 loop

      if I mod 2 = 0 then

   end loop;

end Main;
"""

exp2 = """procedure Main is

   Even_Number_Count, Odd_Number_Count : Natural := Natural'First;

begin

   for I in 1 .. 100 loop

end Main;
"""


@run_test_driver
def driver():
    b = GPS.EditorBuffer.get(GPS.File("main.adb"))
    yield wait_tasks()

    # preparations and initial checks
    b.add_special_line(20, "special line")
    b.add_special_line(18, "special line")
    b.add_special_line(10, "special line")
    b.add_special_line(8, "special line")
    yield wait_idle()

    gps_assert(b.debug_dump_all_lines(),
               ['[0] el:1',
                '[0] el:2',
                '[0] el:3',
                '[0] el:4',
                '[0] el:5',
                '[0] el:6',
                '[0] el:7',
                '[0] special:el:0',
                '[0] el:8',
                '[0] el:9',
                '[0] special:el:0',
                '[0] el:10',
                '[0] el:11',
                '[0] el:12',
                '[0] el:13',
                '[0] el:14',
                '[0] el:15',
                '[0] el:16',
                '[0] el:17',
                '[0] special:el:0',
                '[0] el:18',
                '[0] el:19',
                '[0] special:el:0',
                '[0] el:20',
                '[0] el:21',
                '[0] el:22'],
               "the check failed")

    # fold 'if' statment at line 9 and check
    yield wait_until_true(
        lambda: b.has_blocks_information())

    b.at(9, 1).block_fold()
    yield wait_idle()

    gps_assert(b.debug_dump_all_lines(),
               ['[0] el:1',
                '[0] el:2',
                '[0] el:3',
                '[0] el:4',
                '[0] el:5',
                '[0] el:6',
                '[0] el:7',
                '[0] special:el:0',
                '[0] el:8',
                '[0] el:9 (8)',
                '[0] special:el:0',
                '[1] el:10',
                '[1] el:11',
                '[1] el:12',
                '[1] el:13',
                '[1] el:14',
                '[1] el:15',
                '[1] el:16',
                '[1] el:17',
                '[0] special:el:0',
                '[0] el:18',
                '[0] el:19',
                '[0] special:el:0',
                '[0] el:20',
                '[0] el:21',
                '[0] el:22'],
               "the check failed")
    gps_assert(b.get_chars(include_hidden_chars=False), exp1,
               "wrong code after the first folding")

    # fold 'for' statment at line 7 and check
    yield wait_until_true(
        lambda: b.has_blocks_information())

    b.at(7, 1).block_fold()
    yield wait_idle()

    gps_assert(b.debug_dump_all_lines(),
               ['[0] el:1',
                '[0] el:2',
                '[0] el:3',
                '[0] el:4',
                '[0] el:5',
                '[0] el:6',
                '[0] el:7 (12)',
                '[0] special:el:0',
                '[1] el:8',
                '[1] el:9 (8)',
                '[0] special:el:0',
                '[2] el:10',
                '[2] el:11',
                '[2] el:12',
                '[2] el:13',
                '[2] el:14',
                '[2] el:15',
                '[2] el:16',
                '[2] el:17',
                '[0] special:el:0',
                '[1] el:18',
                '[1] el:19',
                '[0] special:el:0',
                '[0] el:20',
                '[0] el:21',
                '[0] el:22'],
               "the check failed")
    gps_assert(b.get_chars(include_hidden_chars=False), exp2,
               "wrong code after the second folding")

    # unfold 'for' statment at line 7 and check
    # that statment is unfolded and 'if' statment
    # is still folded
    yield wait_until_true(
        lambda: b.has_blocks_information())

    b.at(8, 1).block_unfold()
    yield wait_idle()

    gps_assert(b.debug_dump_all_lines(),
               ['[0] el:1',
                '[0] el:2',
                '[0] el:3',
                '[0] el:4',
                '[0] el:5',
                '[0] el:6',
                '[0] el:7',
                '[0] special:el:0',
                '[0] el:8',
                '[0] el:9 (8)',
                '[0] special:el:0',
                '[1] el:10',
                '[1] el:11',
                '[1] el:12',
                '[1] el:13',
                '[1] el:14',
                '[1] el:15',
                '[1] el:16',
                '[1] el:17',
                '[0] special:el:0',
                '[0] el:18',
                '[0] el:19',
                '[0] special:el:0',
                '[0] el:20',
                '[0] el:21',
                '[0] el:22'],
               "the check failed")
    gps_assert(b.get_chars(include_hidden_chars=False), exp1,
               "wrong code after unfolding")
