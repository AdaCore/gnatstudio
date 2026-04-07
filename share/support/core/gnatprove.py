"""
This file provides support for using the GNATprove tool.

Specifically, it contains support utilities for the plugins that provide the
various SPARK menus in GNAT Studio.
"""

############################################################################
# No user customization below this line
############################################################################

from dataclasses import dataclass
import os

import GPS
from lal_utils import get_enclosing_subprogram
import libadalang as lal

# Name of the default console
MESSAGES = "Messages"

# Message categories for GNAT Studio

# Messages related to running gnatprove itself
GNATPROVE_MAIN_CATEGORY = "GNATprove"

# Messages related to generating an executable test from a failed check
GNATPROVE_TEST_GEN_CATEGORY = "GNATprove Harness Generation"

# Messages related to generating counterexamples via an external tool such as
# gnattest and/or gnatfuzz
GNATPROVE_CE_GEN_CATEGORY = "GNATprove Counterexample Generation"

# Name of the trace used for logging SPARK diagnostics from GNAT Studio. Can be
# activated e.g. by the user or the testsuite.
SPARK_GS_TRACE = "PYTHON.SPARK"


logger = GPS.Logger(SPARK_GS_TRACE)


@dataclass
class ContextData:
    """Various information about the context of the check"""

    check_file: str
    check_line: int
    check_col: int
    spec_file: str
    spec_line: int
    project_name: str
    project_path: str
    unit_name: str
    artifacts_dir: str


class ExternalProcessError(RuntimeError):
    def __init__(self, message):
        super().__init__(message)


class ContextError(RuntimeError):
    def __init__(self, message):
        super().__init__(message)


def get_context_data(context):
    """
    Extract and populate a ContextData record with various information about
    the context of the check.

    :param context: GPS context for the check.

    :raises ContextError: If the context is not supported.
    """
    check_loc = str(context.location())
    logger.log(f"get_context_data: check_loc={check_loc}")

    logger.log("get_context_data: trying to get gps_project ...")
    gps_project = context.project()
    logger.log(f"get_context_data: gps_project={gps_project}")

    # Look up the GPS project.
    # Note: In aggregate projects it is important to stay within the GPS
    # project of the given source file (and not go up to the root project) as
    # this matches the the behavior or GNATprove and GNATtest in the use cases
    # that are relevant here.
    project_name = str.lower(gps_project.name())
    project_path = gps_project.file().path
    logger.log(
        f"get_context_data: project_name={project_name}, project_path={project_path}"
    )

    # Determine the GPS location of the spec.
    gps_spec_loc = spec_location(context)
    if not gps_spec_loc:
        raise ContextError("Unsupported context. Expecting a subprogram.")

    spec_loc = str(gps_spec_loc)
    logger.log(f"get_context_data: spec_loc={spec_loc}")

    spec_file, spec_line, spec_col = split_location(spec_loc)
    check_file, check_line, check_col = split_location(check_loc)

    unit_name = os.path.splitext(os.path.basename(spec_file))[0]

    artifacts_dir = gps_project.artifacts_dir()

    config = ContextData(
        check_file=check_file,
        check_line=check_line,
        check_col=check_col,
        spec_file=spec_file,
        spec_line=spec_line,
        project_name=project_name,
        project_path=project_path,
        unit_name=unit_name,
        artifacts_dir=artifacts_dir,
    )

    return config


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


def current_subprogram(context):
    """
    Return the LAL node corresponding to the subprogram enclosing the current
    context, or None
    """

    curloc = context.location()

    # Return None when called from non-editor context
    if not GPS.EditorBuffer.get(curloc.file(), open=False):
        return None

    # Handle to the project associated with the current context and source file.
    # Note: In aggregate projects this isn't necessarily the root project
    # that is loaded to GNAT Studio.
    gps_project = context.project()

    # In aggregate projects the unit provider that is associated with the
    # editor buffer cannot reliably resolve the subprogram's declaration.
    # Hence, a dedicated unit provider that is aware of both both projects is
    # created and used next.
    unit_provider = lal.UnitProvider.for_project(
        GPS.Project.root().file().path, project=gps_project.file().path
    )
    analysis_context = lal.AnalysisContext(unit_provider=unit_provider)

    # Get the analysis unit for the current source file
    unit = analysis_context.get_from_file(curloc.file().path)
    if not unit.root:
        logger.log(f"failed to load unit {curloc.file().path()!r}")
        if unit.diagnostics:
            logger.log(
                "unit diagnostics:\n" + "\n".join([str(d) for d in unit.diagnostics])
            )
        return None
    node = unit.root.lookup(lal.Sloc(curloc.line(), curloc.column()))

    return get_enclosing_subprogram(node)


def spec_location(context) -> GPS.EditorLocation | None:
    """
    Return GPS.EditorLocation for the spec of the subprogram enclosing the
    context node, or None when the context is not enclosed in a subprogram
    """

    subprogram_node = current_subprogram(context)
    logger.log(f"spec_location: Current subprogram_node {subprogram_node}")
    if subprogram_node:
        decl_part = subprogram_node.p_decl_part()
        logger.log(f"spec_location: Current decl_part {decl_part}")
        if decl_part:
            # The original context was in the subprogram body
            return decl_part.gps_location()
        else:
            # The original context was in the subprogram spec, e.g., expression
            # function. Return the location of the current subprogram.
            return subprogram_node.gps_location()
    else:
        # Unsupported context. Enclosing subprogram not found.
        return None


def split_location(location: str) -> tuple[str, int, int]:
    """
    Split location to (file, line, col)

    :param location: Location string. E.g.:
        * 'xyz.ads:3:4'
        * '/home/me\\xyz.ads:3:4'
        * 'C:\\Users\\Me\\src\\xyz.ads:3:4'

    :raises ContextError: If the context is not supported.
    """
    if not location:
        raise ContextError("Location string cannot be empty.")

    # Split from the right exactly 2 times to isolate line and col
    parts = location.rsplit(":", 2)

    if len(parts) != 3:
        raise ContextError(
            f"Unsupported location context: '{location}'. Expected 'file:line:col'."
        )

    file_path, line_str, col_str = parts

    if not file_path:
        raise ContextError("File path cannot be empty.")

    try:
        line = int(line_str)
        col = int(col_str)
    except ValueError:
        raise ContextError(
            f"Line and column must be integers. Got line: '{line_str}', col: '{col_str}'."
        )

    return file_path, line, col
