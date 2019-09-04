import GPS
import json
from gps_utils.internal.utils import run_test_driver, gps_assert, simple_error
from workflows.promises import timeout


@run_test_driver
def driver():
    q_ads = GPS.File("q.ads")
    p_ads = GPS.File("p.ads")
    b = GPS.EditorBuffer.get(q_ads)
    als = GPS.LanguageServer.get_by_language_name("Ada")

    params = {"textDocument": {"uri": q_ads.uri},
              "position": {"line": 2, "character": 27},
              "context": {"includeDeclaration": True}}

    result = yield als.request_promise("textDocument/references", params)

    if not result.is_valid:
        simple_error("we were expecting a valid result")

    # The result we expect from find_all_references
    expected = [{"uri": q_ads.uri,
                 "range": {"start": {"line": 2, "character": 27},
                           "end": {"line": 2, "character": 30}},
                 "alsKind": ["call"]},
                {"uri": p_ads.uri,
                 "range": {"start": {"line": 1, "character": 12},
                           "end": {"line": 1, "character": 15}}}]

    gps_assert(result.data, expected,
               "result contents doesn't match expectations")
