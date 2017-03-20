"""
This file provides libAdaLang integration in GPS.

"""

###########################################################################
# No user customization below this line
###########################################################################

import libadalang


def _location(self, open=True):
    """
    Return GPS.EditorLocation corresponding to start of given AdaNode.
    If open=False and there is no open editor with the unit, return None.
    """
    file = GPS.File(self.unit.filename)
    buffer = GPS.EditorBuffer.get(file, open=open)

    if buffer:
        start = self.token_start.sloc_range.start
        return buffer.at(int(start.line), int(start.column))
    else:
        return None

# Register new method in AdaNode
libadalang.AdaNode.gps_location = _location
