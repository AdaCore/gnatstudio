"""
This is the python format checker plugin for GPS.
Its aim is to check identatoin, style when:
 1 user stop editing
 2 before python script is saved
"""

from GPS import *
import tool_output
import GPS
import colorschemes
import sys
from cStringIO import StringIO
import os
from modules import Module

PEP8_IMPORTED = 1

try:
    import pep8
except ImportError:
    PEP8_IMPORTED = 0


class Catch_Stdout(list):
    def __enter__(self):
        self._stdout = sys.stdout
        sys.stdout = self._stringio = StringIO()
        return self

    def __exit__(self, *args):
        self.extend(self._stringio.getvalue().splitlines())
        sys.stdout = self._stdout


class Pep8_Module(Module):

    def __format_check(self, file):
        """
           Check format using pep8 for python files
        """
        if file.language() == "python":
            fname = "/tmp/_gps_pep8.py"
            for m in GPS.Message.list(category="Pep8"):
                m.remove()
            f = open(fname, 'w')
            f.write(GPS.EditorBuffer.get(file).get_chars())
            f.close()
            with Catch_Stdout() as output:
                pep8.StyleGuide().input_file(fname)

            if len(output) > 0:
                for i in output:
                    a = i.split(":")
                    m = GPS.Message(category="Pep8",
                                    file=GPS.EditorBuffer.get().file(),
                                    line=int(a[1]),
                                    column=int(a[2]),
                                    text=a[3],
                                    flags=2)
                    m.set_action("", "gps-build-warning", m.get_text())
                    m.set_style(colorschemes.STYLE_WARNING, 1)

            os.remove(fname)

    # The followings are hooks:
    def gps_started(self):
        """
           When GPS start, if imported success:
           register hook for format checker
        """
        if PEP8_IMPORTED:
            GPS.Hook("before_file_saved").add(self.on_file_saved)

    def on_file_saved(self, hook, f):
        """
           When file is saved, check the format
        """
        if PEP8_IMPORTED:
            self.__format_check(f)

    def buffer_edited(self, f):
        """
           When user stop editing, check the format
        """
        if PEP8_IMPORTED:
            self.__format_check(f)
