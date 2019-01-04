"""Verify that the editor is scrolled to show the target location when
jumping to declaration or jumping to body"""

import os
from gps_utils.internal.utils import run_test_driver, wait_tasks, timeout,\
    gps_assert
from pygps import get_widgets_by_type


VADJ_LOWER_BOUND = 400

@run_test_driver
def driver():
    GPS.BuildTarget("Build All").execute()
    yield wait_tasks()

    b = GPS.EditorBuffer.get(GPS.File("pr.adb"))
    v = b.current_view()
    v.goto(b.at(4, 4))

    GPS.execute_action("goto declaration")
    yield wait_tasks()

    bla_ads_buf = GPS.EditorBuffer.get()
    gps_assert(os.path.basename(bla_ads_buf.file().name()),
               "bla.ads",
               "Goto declaration didn't jump to the right file")

    bla_ads_view = bla_ads_buf.current_view()

    text_view = get_widgets_by_type(Gtk.TextView, bla_ads_view.pywidget())[0]

    adj_value = text_view.get_vadjustment().get_value()
    adj_max = text_view.get_vadjustment().get_upper()

    # Verify that we're scrolled vertically
    gps_assert(adj_value > VADJ_LOWER_BOUND,
               True,
               "The text is not scrolled after jumping to declaration")

    GPS.execute_action("goto body")
    yield wait_tasks()

    bla_adb_buf = GPS.EditorBuffer.get()
    gps_assert(os.path.basename(bla_adb_buf.file().name()),
               "bla.adb",
               "Goto body didn't jump to the right file")

    bla_adb_view = bla_ads_buf.current_view()

    text_view = get_widgets_by_type(Gtk.TextView, bla_adb_view.pywidget())[0]

    adj_value = text_view.get_vadjustment().get_value()
    adj_max = text_view.get_vadjustment().get_upper()

    # Verify again that we're scrolled vertically
    gps_assert(adj_value > VADJ_LOWER_BOUND,
               True,
               "The text is not scrolled after jumping to body")
