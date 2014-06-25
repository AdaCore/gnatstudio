"""
This is the python format checker plugin for GPS.
Its aim is to check identatoin, style when:
 1 user stop editing
 2 before python script is saved
"""

import os
import sys
from itertools import chain
import pep8
from GPS import *
import tool_output
import GPS
import colorschemes
from cStringIO import StringIO
from modules import Module


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
           Check format using pep8 for python source codes
        """
        print file
        # only check python file
        if file.language() == "python":
            for m in GPS.Message.list(category="Pep8"):
                m.remove()

            # parse text in buffer and catches stdout
            source = [i+"\n" for i in
                      GPS.EditorBuffer.get(file=file, open=False).
                      get_chars().splitlines()]

            with Catch_Stdout() as output:
                m = pep8.Checker(filename=None, lines=source, report=False)
                m.check_all()

            for i in output:
                a = i.split(":")
                m = GPS.Message(category="Pep8",
                                file=GPS.EditorBuffer.get(file=file,
                                                          open=False).file(),
                                line=int(a[1]),
                                column=int(a[2]),
                                text=a[3],
                                flags=2)

                m.set_action("", "gps-build-warning", m.get_text())
                m.set_style(colorschemes.STYLE_WARNING, 1)

    # The followings are hooks:
    def gps_started(self):
        """
           When GPS start, if imported success:
           register hook for format checker
        """
        GPS.Hook("before_file_saved").add(self.on_file_saved)

    def on_file_saved(self, hook, f):
        """
           When file is saved, check the format
        """
        self.__format_check(f)

    def buffer_edited(self, f):
        """
           When user stop editing, check the format
        """
        self.__format_check(f)
