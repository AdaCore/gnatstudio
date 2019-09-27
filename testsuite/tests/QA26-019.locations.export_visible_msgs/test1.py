"""
Set the 'locations-save-in-desktop' preference to True and
run 'Build All': this should produce warning messages.
"""

from GPS import *
from gps_utils.internal.utils import *

@run_test_driver
def run_test():
    GPS.EditorBuffer.get(GPS.File("a.adb"))
    GPS.Preference("locations-save-in-desktop").set(True)
    GPS.Message(category="Unknown",
                file=GPS.File("a.adb"),
                line=1,
                column=1,
                text="Blabla",
                show_in_locations=True,
                importance=GPS.Message.Importance.HIGH)
    GPS.Message(category="Unknown",
                file=GPS.File("a.adb"),
                line=2,
                column=1,
                text="Blabla",
                show_in_locations=False,
                importance=GPS.Message.Importance.HIGH)
    yield timeout(4000)
