"""
Check switching project in GPS
"""
from gs_utils.internal.utils import run_test_driver, wait_tasks

@run_test_driver
def test_driver():
    server = GPS.LanguageServer.get_by_language_name ("ada")
    yield wait_tasks()

    GPS.Project.load('../b/p2.gpr')
    server = GPS.LanguageServer.get_by_language_name ("ada")
    yield wait_tasks()
