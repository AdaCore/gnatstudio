"""
This test checks that
1) closing an unnamed buffer shows a confirmation dialog
   even when there is another unnamed buffer.
2) "Save All" action shows a confirmation dialog with
   both unnamed buffers in its list.
"""

import GPS
import pygps
from gs_utils.internal.utils import *

# A close confirmation dialog
class Confirmation(Dialog):

    def open_and_yield(self):
        yield self._open_and_yield('Close current window')
        self.editor = self.dialogs[0]

    def cancel(self):
        if self.editor:
            get_button_from_label("gtk-cancel", self.editor).clicked()

# "Save All" confirmation dialog
class Save_All(Dialog):

    def open_and_yield(self):
        yield self._open_and_yield('save files and projects')
        self.editor = self.dialogs[0]
        self.tree = get_widgets_by_type(Gtk.TreeView, self.editor)[0]

    def cancel(self):
        if self.editor:
            get_button_from_label("gtk-cancel", self.editor).clicked()

@run_test_driver
def run_test():
    # Create two unnamed buffers
    GPS.execute_action("new file")
    buffer_one = GPS.EditorBuffer.get()
    buffer_one.insert("First")
    GPS.execute_action("new file")
    buffer_two = GPS.EditorBuffer.get()
    buffer_two.insert("Second")

    # Try to close first one and see if we have a confirmation dialog
    confirm = Confirmation()
    yield confirm.open_and_yield()
    gps_not_null(confirm.editor)
    confirm.cancel()

    # Execute "Save All" and check buffers to save
    saveall = Save_All()
    yield saveall.open_and_yield()
    model = saveall.tree.get_model()
    files = [x[1] for x in model]
    files.sort()
    gps_assert(files, ['Untitled', 'Untitled (1)'])
    saveall.cancel()

