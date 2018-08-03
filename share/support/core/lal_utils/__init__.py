import GPS
import lal_view

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
