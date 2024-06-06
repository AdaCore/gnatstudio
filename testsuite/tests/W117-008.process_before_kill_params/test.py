from gs_utils.internal.utils import run_test_driver


def before_kill(self, text):
    pass


@run_test_driver
def driver():
    GPS.Process("echo", before_kill=before_kill)
