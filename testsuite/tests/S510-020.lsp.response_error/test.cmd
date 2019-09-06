# We pass --traceoff=GPS.LSP_CLIENT.ERRORS because we *do* expect
# an error as part of this test, and we verify in test.py that
# this is reported.
$GPS --traceon=GPS.LSP.ADA_SUPPORT --traceoff=GPS.LSP_CLIENT.ERRORS --load=python:test.py
