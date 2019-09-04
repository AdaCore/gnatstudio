import GPS
import json
from gps_utils.internal.utils import run_test_driver, gps_assert, simple_error
from gps_utils import hook
from workflows.promises import timeout


@run_test_driver
def driver():
    # Generate a giant file on disk and load it from disk
    with open("p.ads", "wb") as f:
        f.write("""package P is
   function Foo return Integer is (42);
end P;""" + ("--" + "spam" * 100 + "\n") * 10000)

    # Reload the project so that p.ads is considered as a source
    GPS.execute_action("reload project")

    q_ads = GPS.File("q.ads")
    p_ads = GPS.File("p.ads")
    a = GPS.EditorBuffer.get(p_ads)
    yield timeout(1000)
    b = GPS.EditorBuffer.get(q_ads)

    # Now insert a huge amount of data in an already open editor
    b.insert(b.at(4, 7), ("--  " + "spam" * 100 + "\n") * 10000)

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

    gps_assert(
        result.data, expected,
        "result contents doesn't match expectations: {}".format(result))
