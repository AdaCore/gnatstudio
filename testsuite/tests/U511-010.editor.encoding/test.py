# -*- coding: utf-8 -*-
"""
Send an UTF-8 non ASCII character in an editor
"""
import GPS
from gs_utils.internal.utils import *


@run_test_driver
def test_driver():
    buf = GPS.EditorBuffer.get(GPS.File("main.adb"))
    pygps.send_key_event(ord("à"))
