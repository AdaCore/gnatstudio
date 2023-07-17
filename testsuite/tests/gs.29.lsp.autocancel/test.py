"""
Send multiple 'long' requests and verify that only one of them is executed
at a time. Queuing another request should cancel the previous one.
"""

import GPS
from gs_utils.internal.utils import *

REQUEST = "textDocument/documentSymbol"
PARAM_TEMPLATE = '{"query":"","textDocument":{"uri":"file://%s"}}'


@run_test_driver
def driver():
    yield wait_idle()
    f = GPS.File("t.adb")
    als = GPS.LanguageServer.get_by_file(f)

    def on_result_message(*args):
        pass

    def on_error_message(*args):
        simple_error("exception when executing the request:\n%s" % str(args))

    for i in range(0, 10):
        als.request_low_level(method=REQUEST,
                              params=PARAM_TEMPLATE % f.path.replace("\\", "/"),
                              on_result_message=on_result_message,
                              on_error_message=on_error_message,
                              auto_cancel=True)
        gps_assert(len(als.get_requests()),
                   1,
                   "Only one request should be queued")
        if i % 3 == 0:
            yield wait_language_server(REQUEST, "Ada")
