"""
This plugin adds 3 actions:
    - Copy the full path of the current file to the clipboard
    - Copy the full path of the current dir to the clipboard
    - Copy the name of the current file to the clipboard
"""

import GPS
import os
from gs_utils import make_interactive

MENU_PATH = "Edit/Clipboard/"
CONTEXTUAL_PATH = "Clipboard/"


def file_path_to_clipboard():
    "Copy the full path of the current path to the clipboard"
    GPS.Clipboard.copy(GPS.current_context().file().name())


def dir_to_clipboard():
    "Copy the full path of the current directory to the clipboard"
    GPS.Clipboard.copy(GPS.current_context().directory())


def name_to_clipboard():
    "Copy the name of the current path to the clipboard"
    GPS.Clipboard.copy(os.path.basename(GPS.current_context().file().name()))


def custom_make_interactive(name, action):
    make_interactive(callback=action,
                     name=name,
                     category="Clipboard",
                     filter="File",
                     menu=MENU_PATH + name,
                     contextual=CONTEXTUAL_PATH + name)


custom_make_interactive("Copy file path", file_path_to_clipboard)
custom_make_interactive("Copy directory path", dir_to_clipboard)
custom_make_interactive("Copy file name", name_to_clipboard)
