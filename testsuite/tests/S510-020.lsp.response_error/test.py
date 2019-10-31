# -*- coding: utf-8 -*-

""" Test for reporting of error for unknown request """
import GPS
from gs_utils.internal.utils import run_test_driver, simple_error
from workflows.promises import timeout

@run_test_driver
def driver():
    als = GPS.LanguageServer.get_by_language_name("Ada")

    params = {}
    result = yield als.request_promise("textDocument/unknown_request", None)

    if not result.is_error:
        simple_error("we were expecting an error response")
    msg = GPS.Console("Messages").get_text()

    if "The language server has reported the following error" not in msg:
        simple_error("error response must be displayed in Messages view")
