"""
Check the highlighting of assembly files.
"""

from GPS import *
from gs_utils.internal.utils import *


EXPECTED = """keywords_hl 1:1 1:18
numbers_hl 1:22 1:22
comments_hl 1:60 1:74
"""


@run_test_driver
def run_test():
    # Test ASM file
    asm_file = GPS.File("foo.s")
    buf = GPS.EditorBuffer.get(asm_file)
    yield wait_idle()
    gps_assert(asm_file.language().lower(),
               "asm",
               "Wrong language for ASM file")
    gps_assert(get_all_tags(buf),
               EXPECTED,
               "Issue for syntax hightlighting in ASM file")

    # Test ASM2 file
    asm2_file = GPS.File("bar.asm")
    buf = GPS.EditorBuffer.get(asm2_file)
    yield wait_idle()
    gps_assert(asm2_file.language().lower(),
               "asm2",
               "Wrong language for ASM2 file")
    gps_assert(get_all_tags(buf),
               EXPECTED,
               "Issue for syntax hightlighting in ASM2 file")
