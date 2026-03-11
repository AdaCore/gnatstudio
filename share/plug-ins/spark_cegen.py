"""
This plugin provides the functionality to call either GNATtest or GNATfuzz to
generate test cases that can be supplied as counterexample candidates to
GNATprove. Both test case generation workflows require GNATtest being available
since they use the shared JSON format for test cases.

A prerequisite is an unproven, i.e., potentially failing, SPARK check found by
GNATprove.
"""

from dataclasses import dataclass
from glob import glob
import json
import os

import GPS
from gnatprove import (
    GNATPROVE_CE_GEN_CATEGORY,
    logger,
    ExternalProcessError,
    print_error,
    print_info,
    print_newline,
    print_warning,
    spec_location,
)
from spark_ce2test import split_location, get_gnattest_hash, run_gnattest
from workflows.promises import ProcessWrapper, TargetWrapper

# The following target names must be unique. See similar constants in
# spark_ce2test.py.

# GNATtest target to use when use_fuzzing=False
GNATTEST_TARGET = "Run GNATtest to find counterexample candidates"
# GNATtest target to use when use_fuzzing=True
GNATFUZZ_TARGET = "Run GNATfuzz to find counterexample candidates"


@dataclass
class Config:
    """A configuration record holding parameters for test generation."""

    artifacts_dir: str
    project_name: str
    project_path: str
    unit_name: str
    hash_value: str
    spec_file: str
    spec_line: str
    check_file: str
    check_line: str
    check_col: str


class ValueNotFoundError(RuntimeError):
    def __init__(self, message):
        super().__init__(message)


def _extract_ce_candidates(config: Config):
    """
    Extracts counterexample candidates from the GNATtest JSON output
    and writes them to a .ce file.
    """
    test_values = _get_test_case_from_gnattest_json(
        config.artifacts_dir, config.unit_name, config.hash_value
    )

    if not test_values:
        print_warning(
            "No test values found for " + f"{config.spec_file}:{config.spec_line}."
        )
        return

    ce_values_path = os.path.join(
        config.artifacts_dir,
        "gnatprove",
        f"{config.project_name}_{config.unit_name}_{config.spec_line}.ce",
    )

    with open(ce_values_path, "w") as f:
        json.dump(test_values, f)

    print_info(
        "Counterexample candidates have been generated "
        f"and can be found at {ce_values_path}"
    )

    GPS.Message(
        GNATPROVE_CE_GEN_CATEGORY,
        GPS.File(config.check_file),
        config.check_line,
        config.check_col,
        "Counterexample candidates have been generated "
        f"and can be found at {ce_values_path}",
    )


def _fuzz(sub_id: int, force: bool):
    """
    Open a dialog to call GNATfuzz for generating 'better' test values for the
    given subprogram.

    The call parameters can be tweaked by the user.

    :param sub_id: GNATfuzz ID for the subprogram to fuzz.
    :param force: If True, the dialog is suppressed and the command is launched
        directly.
    """

    runner = TargetWrapper(GNATFUZZ_TARGET)
    status = yield runner.wait_on_execute(
        extra_args=[f"--subprogram-ids-to-test={sub_id}", "--disable-styled-output"],
        force=force,
    )
    logger.log(f"_fuzz: finished with status={status}")

    if status != 0:
        raise ExternalProcessError(f"Call to gnatfuzz failed with exit code {status}")


def _get_gnatfuzz_subprogram_id(config: Config):
    """
    Retrieve the subprogram's ID that GNATfuzz uses to refer to the subprogram

    Retrieves the subprogram's ID that GNATfuzz uses to refer to the subprogram
    enclosing the given source location. The call to GNATfuzz occurs in the
    background. Parameters cannot be tweaked by the user.

    Exits with
        * ExternalProcessError when the call to gnatfuzz fails.
        * FileNotFoundError when the analysis result file is not found.
        * JSONDecodeError when the analysis result file cannot be decoded.
        * ValueNotFoundError when the given subprogram is not found among the
            fuzzable subprograms in the analysis result.
    """
    # FIXME These local vars can be inlined ?
    filename = config.spec_file
    line = config.spec_line
    artifacts_dir = config.artifacts_dir

    command = [
        "gnatfuzz",
        "fuzz-everything",
        "inspect",
        f"-P{config.project_path}",
        "--disable-styled-output",
    ]
    logger.log(f"Calling gnatfuzz to get subprogram id: {' '.join(command)}")

    proc = ProcessWrapper(command)

    status, output = yield proc.wait_until_terminate(show_if_error=True)
    logger.log(f"gnatfuzz output: {output}")
    logger.log(f"gnatfuzz finished with result: {status}")

    if status != 0:
        print_error(output)
        raise ExternalProcessError(f"Call to gnatfuzz failed with exit code {status}")

    # TODO: eng/spark/spark2014#1140 - Also check the issues.log.
    # This is not yet possible (cf. eng/das/fuzz/gnatfuzz#1120).

    json_path = os.path.join(artifacts_dir, "gnatfuzz", "analyze.json")

    id = None
    with open(json_path, "r") as f:
        data = json.load(f)

        for sub in data.get("fuzzable_subprograms", {}):

            start_line = sub.get("start_line", {})
            source_filename = sub.get("source_filename", {})

            if str(start_line) == str(line) and filename in source_filename:
                id = sub.get("id", {})
                break

    if id is not None:
        logger.log(f"Found id for the given subprogram: {id} ")
        yield id
    else:
        logger.log("Id for the given subprogram not found")

        # The given subprogram was not found among the fuzzable subprograms.
        # TODO If this is a nested subprogram, we could go one level up and see
        # if the parent can be fuzzed etc.

        raise ValueNotFoundError(
            f"Subprogram defined at {filename}:{line} is not"
            " among the subprograms fuzzable by GNATfuzz."
            "\n"
            f"Full results of GNATfuzz analysis are in: {json_path}"
        )


def _get_test_case_from_gnattest_json(gnattest_dir, unit_name, subp_hash):
    """Extract test vectors for the given subprogram from GNATtest JSON file"""

    json_file = os.path.join(
        gnattest_dir,
        "gnattest",
        "tests",
        "JSON_Tests",
        f"{unit_name}.json",
    )

    logger.log(f"Extracting test vectors for subprogram {subp_hash} from {json_file!r}")

    try:
        with open(json_file, "r") as f:
            return json.load(f).get(subp_hash, {}).get("test_vectors", {})

    except FileNotFoundError:
        print_error(f"Filename {json_file!r} not found.")

        # Dump the list of any other existing JSON_Tests file to the log for
        # debugging. But exit with an error nonetheless.
        path_pattern = os.path.join(gnattest_dir, "**", "JSON_Tests", "**", "*.json")
        candidates = glob(path_pattern, recursive=True)
        if candidates:
            file_list_str = "\n".join(candidates)
            logger.log(f"Other JSON_Tests files found:\n{file_list_str}")

        return None

        return None
    except json.JSONDecodeError:
        print_error(f"Invalid JSON format in {json_file!r}.")
        return None
    except Exception as e:
        print_error(f"An unexpected error occurred: {e}")
        return None


def run(
    context,
    use_fuzzer: bool,
    force: bool = False,
):
    """
    Generate candidate counterexamples for the given subprogram using either
    GNATtest or GNATfuzz

    Note: This function needs to be called as a GNAT Studio workflow.

    :param context: GPS context for the check.
        Note: The 'context' object passed into this function cannot be reliably
        used after calling this function.
    :param use_fuzzer: If True, use GNATfuzz for finding the test cases,
        otherwise use GNATtest's internal heuristics.
    :param force: Flag passed to GPS.BuildTarget. If True, all dialogs are
        suppressed and the processes are launched directly.
    """

    # Remove any previous messages related to test generation

    GPS.Locations.remove_category(GNATPROVE_CE_GEN_CATEGORY)

    print_info("Starting to generate counterexamples...")

    # Extract location details

    check_loc = str(context.location())
    logger.log(f"cegen.run check_loc={check_loc}")

    logger.log("cegen.run trying to get gps_project ...")
    gps_project = context.project()
    logger.log(f"cegen.run gps_project={gps_project}")

    # Look up the GPS project.
    # Note: In aggregate projects it is important to stay within the GPS
    # project of the given source file (and not go up to the root project) as
    # this matches the the behavior or GNATprove and GNATtest in the use cases
    # that are relevant here.
    project_name = str.lower(gps_project.name())
    project_path = gps_project.file().path
    logger.log(f"cegen.run: project_name={project_name}, project_path={project_path}")

    object_dirs = gps_project.object_dirs(recursive=False)

    # Determine the GPS location of the spec.
    # Note: 'context' becomes stale and must not be accessed any more.
    gps_spec_loc = spec_location(context)
    if not gps_spec_loc:
        print_error("Unsupported context. Expecting a subprogram.")
        return

    spec_loc = str(gps_spec_loc)
    logger.log(f"cegen.run spec_loc={spec_loc}")

    spec_file, spec_line, spec_col = split_location(spec_loc)
    check_file, check_line, check_col = split_location(check_loc)

    unit_name = os.path.splitext(os.path.basename(spec_file))[0]

    # Get the hash that GNATtest will use for the enclosing subprogram

    try:
        hash_value = yield get_gnattest_hash(
            project_path,
            os.path.join("src", spec_file),
            spec_line,
        )
    except (ExternalProcessError, ValueError) as e:
        print_error(f"{e}")
        return

    # Find the artifacts directory (the parent of 'gnattest' directory)

    artifacts_dir = None
    for base_dir in object_dirs:
        if os.path.isdir(os.path.join(base_dir, "gnattest")):
            artifacts_dir = base_dir
            break

    if not artifacts_dir:
        print_error("Could not find 'gnattest' dir.")
        return

    config = Config(
        artifacts_dir=artifacts_dir,
        project_name=project_name,
        project_path=project_path,
        unit_name=unit_name,
        hash_value=hash_value,
        spec_file=spec_file,
        spec_line=spec_line,
        check_file=check_file,
        check_line=check_line,
        check_col=check_col,
    )

    if use_fuzzer:
        # Use the fuzzer to generate some potentially interesting test cases
        print_info("Calling gnatfuzz to find potential counterexamples ...")
        try:
            subp_id = yield _get_gnatfuzz_subprogram_id(config)

            print_newline()
            print_info("Calling gnatfuzz to find counterexample values ...")

            yield _fuzz(subp_id, force)

            print_info("Fuzzing completed")

        except Exception as e:
            print_error(f"{e}")
            return
    else:
        # Use the gnattest's heuristics to generate some test cases
        print_info("Calling gnattest to generate test cases ...")
        try:
            yield run_gnattest(
                config.spec_file,
                config.spec_line,
                GNATTEST_TARGET,
                force,
            )

            print_info("Test case generation completed")

        except ExternalProcessError as e:
            print_error(f"{e}")
            return

    # Extract counterexample candidates
    _extract_ce_candidates(config)
