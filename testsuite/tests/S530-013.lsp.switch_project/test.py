"""
Check switching project in GPS
"""
from gps_utils.internal.utils import run_test_driver, wait_tasks

@run_test_driver
def test_driver():
    server = GPS.LanguageServer.get_by_language_name ("ada")
    yield ('language_server_started')

    yield wait_tasks()
    GPS.Project.load('../b/p2.gpr')
    server = GPS.LanguageServer.get_by_language_name ("ada")
    yield ('language_server_started')
