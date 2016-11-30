#!/usr/bin/python
# -*- coding: utf-8 -*-


import GPS
from gi.repository import Gtk
from menu import TestContextual
from asserts import gps_assert, gps_not_null, gps_assert_menu
import pygps.tree
from workflows.promises import Promise


def find_in_tree(tree, column, key, iter=None):
    """obsolete: use the Tree class instead"""
    result = pygps.tree.find_in_tree(tree, column, key, iter)
    if not result:
        from gps_utils.internal.utils import gps_fatal_error
        gps_fatal_error('Row not found in tree: %s' % (key, ))
    return result


def dump_tree_model(model, column=-1):
    """obsolete: use the Tree class instead"""
    return Tree.dump_tree_model(model, column=column)


class Tree(object):
    """
    Abstract base class for tree-based views
    """

    def __init__(self, treeview):
        """Set the GtkTreeView widget"""
        self.treeview = treeview

    @staticmethod
    def dump_tree_model(model, column=-1):
        """
        Return the model's contents for a specific column
        :param int column: the column to dump, or -1 to compare all of them.
        """
        if not model:
            return None

        value = []
        for row in model:
            if column == -1:
                value.append([r for r in row])
            elif row[column]:
                value.append(row[column])

            iter = row.iterchildren()
            if iter:
                result = Tree.dump_tree_model(iter, column)
                if result != []:
                    value.append(result)
        return value

    def dump_model(self, column=1):
        return Tree.dump_tree_model(self.treeview.get_model(), column)

    def compare_contents(self, expected, column=1, msg=''):
        """
        Compare the contents of the tree with some expected value.
        :param int column: the column to compare in the model
        :param expected: a list of the form
             [node_name, [child_name, [grand_child], child2_name],...]
        """
        gps_assert(
            self.dump_model(column=column),
            expected,
            msg)

    def select_by_name(self, column, value):
        """
        Find the first node in the tree that contains value in the given
        column, and select that row.
        :param int column: the model's column to check.
        :param str value:  the value to find in the model.
        :return GtkTreePath: the path that was selected.
        """
        p = pygps.tree.find_in_tree(self.treeview, column=column, key=value)
        gps_not_null(p, 'Row not found in tree: %s' % (value, ))
        self.treeview.get_selection().select_path(p)
        return p

    def expand_by_name(self, column, value):
        """
        Find the row that contains value in the given column, and expand it
        to show its contents.
        """
        path = self.select_by_name(column=column, value=value)
        self.treeview.expand_row(path, open_all=False)

    def contextual_menu_by_name(self, expected, column, value):
        """
        Select the row that contains value in the given column, and display
        the corresponding contextual menu. This must be used as::
            yield self.contextual_menu_by_name(...)
        :return TestContextual: an instance of the TestContextual menu class
        """
        p = self.select_by_name(column=column, value=value)
        menu = TestContextual(
            lambda: pygps.tree.click_in_tree(self.treeview, path=p, button=3))
        yield menu.open()
        menu.compare(expected)

    def click(self, column, path=None, value=None, *args, **kwargs):
        """
        If path not given, then find the first node in the tree that contains
        value in the given column. Click on the column in a row of the path.
        Return promise to wait selection changed.
        This function accepts other parameters to pass them to click_in_tree,
        such as button, events, process_events, control, alt, shift, modifier.

        Example:

        yield my_tree.click (column=1, value="key", button=1)

        :param int column: the model's column to check.
        :param str path:   the path in the tree to click.
        :param str value:  the value to find in the model, if path not set.
        :return Promise:   the event of selection change.
        """

        if not path:
            path = pygps.tree.find_in_tree(self.treeview,
                                           column=column, key=value)

        p = Promise()

        def handler(selection):
            p.resolve()

        self.treeview.get_selection().connect("changed", handler)
        pygps.tree.click_in_tree(self.treeview, path, *args, **kwargs)

        return p
