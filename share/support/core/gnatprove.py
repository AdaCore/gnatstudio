"""
This file provides support for using the GNATprove tool.

Specifically, it contains support utilities for the plugins that provide the
various SPARK menus in GNAT Studio.
"""

############################################################################
# No user customization below this line
############################################################################

import GPS
from lal_utils import get_enclosing_subprogram
import libadalang as lal

# Name of the default console
MESSAGES = "Messages"

# Category of messages in GNAT Studio
MESSAGES_CATEGORY = "GNATprove"

# Name of the trace used for logging SPARK diagnostics from GNAT Studio. Can be
# activated e.g. by the user or the testsuite.
SPARK_GS_TRACE = "PYTHON.SPARK"


logger = GPS.Logger(SPARK_GS_TRACE)


class UserAbort(Exception):
    pass


class ExternalProcessError(RuntimeError):
    def __init__(self, message):
        super().__init__(message)


def print_error(message) -> None:
    """Print errors to the messages console"""

    logger.log(f"ERROR: {message}")
    GPS.Console(MESSAGES).write("Error: " + message + "\n", mode="error")


def print_warning(message) -> None:
    """Print warnings to the messages console"""

    logger.log(f"WARNING: {message}")
    GPS.Console(MESSAGES).write("Warning: " + message + "\n")


def print_info(message) -> None:
    """Print information to the messages console"""

    logger.log(f"INFO: {message}")
    GPS.Console(MESSAGES).write(message + "\n")


def print_newline() -> None:
    """Print a blank line to the messages console"""

    GPS.Console(MESSAGES).write("\n")


def current_subprogram(self):
    """
    Return the LAL node corresponding to the subprogram enclosing the current
    context, or None
    """

    curloc = self.location()
    buf = GPS.EditorBuffer.get(curloc.file(), open=False)
    if not buf:
        return False
    unit = buf.get_analysis_unit()
    node = unit.root.lookup(lal.Sloc(curloc.line(), curloc.column()))
    return get_enclosing_subprogram(node)


def spec_location(context) -> GPS.EditorLocation | None:
    """
    Return GPS.EditorLocation for the spec of the subprogram enclosing the context node,
    or None when the context is not enclosed in a subprogram
    """

    subprogram_node = current_subprogram(context)
    if subprogram_node:
        previous_part = subprogram_node.p_previous_part_for_decl()
        if previous_part:
            # The original context was in the subprogram body
            return previous_part.gps_location()
        else:
            # The original context was in the subprogram spec, e.g., expression
            # function. Return the location of the current subprogram.
            return subprogram_node.gps_location()
    else:
        # Unsupported context. Enclosing subprogram not found.
        return None
