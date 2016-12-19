#!/usr/bin/python
# -*- coding: utf-8 -*-

from gi.repository import Gtk
from workflows.promises import wait_idle, idle_modal_dialog
from asserts import gps_assert_menu, gps_not_null
import GPS
import pygps


def dump_menu(menupath, topwidget=None, accel_path_prefix='<gps>/'):
    """
    Dump the structure of a menu. For instance:
        dump_menu(GPS.Menu.get("/Edit"))
    :param menupath: The /Build/Project/ path to dump.
    :param topwidget: if unset, will examine the global menubar, otherwise
       could be a widget from a contextual menu for instance.
    """

    def __internal(item, prefix='', add=False):
        result = []
        if (isinstance(item, Gtk.MenuItem) and item.get_visible()):

            label = ''
            if isinstance(item, Gtk.SeparatorMenuItem):
                label = '<separator>'
            else:
                for m in item.get_children():
                    if isinstance(m, Gtk.AccelLabel):
                        menu_accel_path = m.get_text() + '/'
                        key = Gtk.AccelMap.lookup_entry(
                            accel_path_prefix + m.get_text())
                        if key[0] and key[1].accel_key:
                            label = (
                                m.get_text(),
                                Gtk.accelerator_name(key[1].accel_key,
                                                     key[1].accel_mods))
                        else:
                            label = m.get_text()
                        break
                    elif isinstance(m, Gtk.Label):
                        label = m.get_text()
                        break
            if label:
                if isinstance(label, tuple):
                    str = label[0]
                else:
                    str = label

                p = prefix + str + '/'
                a = add or p.startswith(menupath)

                if a:
                    result.append(label)
                if item.get_submenu():
                    children = __internal(
                        item.get_submenu(),
                        prefix=p,
                        add=a)
                    if children:
                        if not result:
                            result = children
                        else:
                            result.append(children)

        elif isinstance(item, Gtk.Container):
            for m in item.get_children():
                result.extend(__internal(m, prefix, add))

        return result

    if not topwidget:
        topwidget = pygps.get_widgets_by_type(Gtk.MenuBar)[0]

    return __internal(topwidget, '/')


class TestContextual(object):
    """
    A wrapper for contextual menus
    """

    def __init__(self, creator):
        self.creator = creator

    def open(self):
        """
        Opens a contextual menu by calling creator::
            yield self.open()
        """
        windows = Gtk.Window.list_toplevels()
        yield idle_modal_dialog(self.creator)
        self.menu = [w for w in Gtk.Window.list_toplevels()
                     if w not in windows and w.get_mapped()]
        if gps_not_null(self.menu, 'Contextual menu not created'):
            self.menu = self.menu[0]

    def compare(self, expected):
        """
        Compare the contents of the menu with an expected output.
        """
        gps_assert_menu(
            expected, dump_menu('', self.menu), 'Contextual menu differs')

    def select(self, label):
        """
        Execute one of the items in the menu, as in:
            yield self.select('A is called by')
        The menu must already have been opened
        """
        if gps_not_null(self.menu, 'Contextual menu not created yet'):
            t = pygps.MenuTree(self.menu, accel_prefix='')
            for (menu, menu_label, accel, level) in t:
                if menu_label == label:
                    yield wait_idle()
                    menu.activate()
                    self.menu.destroy()   # close the contextual menu
                    yield wait_idle()
                    return
            gps_assert(True, False, 'Contextual menu not found "%s" in %s' % (
                label, dump_menu('', self.menu)))
