import GPS
from . import dialogs
from dialogs import Dialog
from gi.repository import Gtk, Gdk
from pygps import get_widgets_by_type
from pygps.tree import *
from workflows.promises import wait_tasks


class Commits(Dialog):

    COLUMN_FILE = 0
    COLUMN_STAGED = 1
    COLUMN_NAME = 2
    COLUMN_INCONSISTENT = 4
    COLUMN_FOREGROUND = 6

    COLOR_TITLE = 'rgb(0,0,102)'
    COLOR_GRAY = 'rgb(0,0,153)'
    COLOR_BLACK = 'rgb(0,0,0)'

    def open_and_yield(self):
        yield self._open_and_yield('open Commits')
        self.view = GPS.MDI.get("Commits")
        self.tree = get_widgets_by_type(Gtk.TreeView, self.view.pywidget())[0]
        self.msg = get_widgets_by_type(Gtk.TextView, self.view.pywidget())[0]

    def dump(self, columns=[COLUMN_NAME]):
        """
        Show the contents of the Commits view
        """
        m = self.tree.get_model()

        def _get_col(iter, col):
            if col == Commits.COLUMN_FOREGROUND:
                v = m[iter][col]
                v = v.to_string()
                if v == Commits.COLOR_TITLE:
                    return 'titleColor'
                elif v == Commits.COLOR_GRAY:
                    return 'grayColor'
                elif v == Commits.COLOR_BLACK:
                    return 'blackColor'
                else:
                    return v
            else:
                return m[iter][col]

        def internal(iter):
            result = []
            while iter is not None:
                row = m[iter]
                result.append(tuple(_get_col(iter, c) for c in columns))
                if m.iter_has_child(iter):
                    result.append(internal(m.iter_children(iter)))
                iter = m.iter_next(iter)
            return result
        return internal(m.get_iter_first())

    def stage(self, files):
        """
        Stage one or more files for commit, by clicking in the tree

        :param [GPS.File] files:
        """
        for f in files:
            select_in_tree(self.tree, column=Commits.COLUMN_FILE, key=f)
        GPS.execute_action('vcs stage file')
        yield wait_tasks()

    def set_message(self, msg):
        b = self.msg.get_buffer()
        b.insert(b.get_start_iter(), msg)

    def commit_staged(self):
        GPS.execute_action('vcs commit staged files')
        yield wait_tasks()


class Branches(Dialog):

    def open_and_yield(self):
        yield self._open_and_yield('open Branches')


class History(Dialog):

    def open_and_yield(self):
        yield self._open_and_yield('open History')
