import GPS
import ctypes
import lal_view
import libadalang as lal

__all__ = [lal_view]


def node(file, line, column, kind_name):
    """Return the node at the given coordinates and with the given kind.

    If the buffer for this file is not open, or the node doesn't exist,
    return None.
    """
    buf = GPS.EditorBuffer.get(file, open=False)
    if not buf:
        return None
    unit = buf.get_analysis_unit()
    results = unit.root.findall(lambda x: x.sloc_range.start.line == line and
                                x.sloc_range.start.column == column and
                                x.kind_name == kind_name)
    if results:
        return results[0]


def get_enclosing_subprogram(node):
    """Return the node that's the innermost enclosing subprogram to node.

    Return None if none is found.
    """
    if not node:
        return None
    enclosing = filter(lambda x: isinstance(x, (lal.SubpBody, lal.SubpSpec)),
                       node.parent_chain)
    # Return the first item in the chain: this is the innermost one
    if enclosing:
        return enclosing[0]
    else:
        return None


def _wrap_analysis_unit(value):
    c_type = ctypes.cast(value, lal.AnalysisUnit._c_type)
    return lal.AnalysisUnit._wrap(c_type)


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
lal.AdaNode.gps_location = _location
