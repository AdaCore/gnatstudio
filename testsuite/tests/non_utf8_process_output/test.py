""" Test a case that might cause a crash when processing
    output from a GPS.Process spawned process: if the output
    is not utf-8 valid, a storage_error might occur as Python
    attempts to create a string object from this.
"""

from gs_utils.internal.utils import run_test_driver, timeout


@run_test_driver
def driver():
    def generic_on_match(process, match, since_last):
        GPS.Console("Messages").write(since_last.strip() + "\n" + match)

    # Launch a process that outputs a non utf-8 string,
    # with a function called on match which writes the text
    # to the console.
    GPS.Process("cat toto.txt", regexp=r".+", on_match=generic_on_match)

    waited = 0

    # The non-utf8 text should not cause a problem and the subsequent
    # text "REACHED THIS" should be present in the console.
    while "REACHED THIS" not in GPS.Console().get_text():
        waited += 1
        if waited > 20:
            GPS.exit(1)
        yield timeout(100)
