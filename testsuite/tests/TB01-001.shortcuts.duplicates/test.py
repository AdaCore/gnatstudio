"""
This test checks all actions with duplicated shortcuts.
"""

import os_utils
import collections
from gs_utils.internal.utils import run_test_driver, gps_assert

expected = {
    'Ctrl+Q': ['no casing/indentation on next key', 'exit'],
    'Escape': ['exit search', 'smart escape'],
    'BackSpace': [
                  'backward delete',
                  'debug tree remove selected variables',
                  'delete file'],
    'Tab': ['tab selection', 'toggle to next alias field']
}

if os_utils.locate_exec_on_path('qgenc'):
    expected['Escape'].insert(0, 'goto parent subsystem')
    expected['Left'] = [
                        'goto previous subsystem',
                        'move to previous char']


@run_test_driver
def driver():
    list = [x for x in GPS.lookup_actions() if GPS.Action(x).get_keys()]

    x = {}
    for item in list:
        key = GPS.Action(item).get_keys()
        if key in x:
            x[key].append(item)
        else:
            x[key] = [item]

    dups = {}
    for j in x:
        if len(x[j]) > 1:
            dups[j] = x[j]

    # This is why we don't try to compare 2 dicts
    gps_assert(len(str(collections.OrderedDict(sorted(dups.items())))),
               len(str(collections.OrderedDict(sorted(expected.items())))),
               "Unexpected duplicated shortcuts")
