import GPS
import ctypes
import lal_view
import libadalang

__all__ = [lal_view]


def node(kind_name, line, column):
    buf = GPS.EditorBuffer.get(open=False)
    if not buf:
        return None
    unit = buf.get_analysis_unit()
    results = unit.root.findall(lambda x: x.sloc_range.start.line == line and
                                x.sloc_range.start.column == column and
                                x.kind_name == kind_name)
    if results:
        return results[0]


def _wrap_analysis_unit(value):
    c_type = ctypes.cast(value, libadalang.AnalysisUnit._c_type)
    return libadalang.AnalysisUnit._wrap(c_type)


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
