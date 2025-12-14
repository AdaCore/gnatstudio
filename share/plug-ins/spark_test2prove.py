"""
This plugin provides the functionality to call GNATtest to generate test cases
that can be supplied as counterexamples candidates to GNATprove. The test case
generation can use optionally either GNATfuzz or GNATtest's internal
heuristics.

A prerequisite is an unproven, i.e., potentially failing, SPARK check found by
GNATprove.
"""

from dataclasses import dataclass
import json
import os
from pathlib import Path

import GPS
from gnatprove import (
    MESSAGES_CATEGORY,
    logger,
    UserAbort,
    ExternalProcessError,
    print_error,
    print_info,
    print_newline,
    print_warning,
)
from spark_testgen import split_location, get_gnattest_hash, run_gnattest

# The following target names must be unique. See similar constants in
# spark_testgen.py.

# GNATtest target to use when use_fuzzing=False
GNATTEST_TARGET1 = "Generate harness and test cases"
# GNATtest target to use when use_fuzzing=True
GNATTEST_TARGET2 = "(Step 1/2) Generate harness and initial test cases"
GNATFUZZ_TARGET = "(Step 2/2) Run GNATFuzz to find counterexamples"


@dataclass
class Config:
    """A configuration record holding parameters for test generation."""

    artifacts_dir: str
    project_name: str
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
        MESSAGES_CATEGORY,
        GPS.File(config.check_file),
        config.check_line,
        config.check_col,
        "Counterexample candidates have been generated "
        f"and can be found at {ce_values_path}",
    )


def _on_fuzzing_exit(config: Config, status: int):
    """
    Callback function executed when the GNATfuzz process exits.
    It checks the status and then extracts the candidates.
    """
    logger.log(f"Fuzzing call completed with status {status}")

    # TODO: eng/spark/spark2014#1140 - Also check the issues.log
    if status:
        print_error(f"Call to gnatfuzz failed with exit code {status}")
        return

    _extract_ce_candidates(config)


def _fuzz_and_continue(config: Config, sub_id: int, force: bool) -> None:
    """
    Open a dialog to call GNATfuzz for generating 'better' test values for the
    given subprogram.

    The call parameters can be tweaked by the user.

    :param config: General configuration.
    :param sub_id: GNATfuzz ID for the subprogram to fuzz.
    :param force: If True, the dialog is suppressed and the command is launched
        directly.

    Exits with UserAbort when the user aborts the dialog, ExternalProcessError
    when the call to gnatfuzz fails.
    """

    GPS.BuildTarget(GNATFUZZ_TARGET).execute(
        extra_args=[f"--subprogram-ids-to-test={sub_id}", "--disable-styled-output"],
        on_exit=lambda status: _on_fuzzing_exit(config, status),
        force=force,
        synchronous=False,
    )


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
    project_name = config.project_name
    filename = config.spec_file
    line = config.spec_line
    artifacts_dir = config.artifacts_dir

    command = [
        "gnatfuzz",
        "fuzz-everything",
        "inspect",
        f"-P{project_name}.gpr",
        "--disable-styled-output",
    ]
    logger.log(f"Calling gnatfuzz to get subprogram id: {' '.join(command)}")

    proc = GPS.Process(command)

    status = proc.wait()
    logger.log(f"gnatfuzz finished with result: {status}")

    # TODO: eng/spark/spark2014#1140 - Also check the issues.log
    if status:
        print_error(proc.get_result())
        raise ExternalProcessError(f"Call to gnatfuzz failed with exit code {status}")

    json_path = os.path.join(artifacts_dir, "gnatfuzz", "analyze.json")

    with open(json_path, "r") as f:
        data = json.load(f)

        for sub in data.get("fuzzable_subprograms", {}):

            start_line = sub.get("start_line", {})
            source_filename = sub.get("source_filename", {})

            if str(start_line) == str(line) and filename in source_filename:
                id = sub.get("id", {})
                logger.log(f"Obtained id: {id}")
                return id

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
        print_error(f"Filename  {json_file}  not found.")
        return None
    except json.JSONDecodeError:
        print_error(f"Invalid JSON format in {json_file}.")
        return None
    except Exception as e:
        print_error(f"An unexpected error occurred: {e}")
        return None


def run(
    spec_loc: str,
    check_loc: str,
    use_fuzzer: bool,
    force: bool = False,
):
    """
    Generate candidate counterexamples for the given subprogram using GNATtest
    and optionally GNATfuzz

    :param spec_loc: Source code location of the spec.
    :param check_loc: Source code location of the given GNATprove check.
    :param use_fuzzer: If True, use GNATfuzz for finding the test cases,
        otherwise use GNATtest's internal heuristics.
    :param force: Flag passed to GPS.BuildTarget. If True, all dialogs are
        suppressed and the processes are launched directly.
    """

    print_info("test2prove starting..." + os.linesep)

    # Extract location details
    spec_file, spec_line, spec_col = split_location(spec_loc)
    check_file, check_line, check_col = split_location(check_loc)

    # Get project and unit names
    project_name = str.lower(GPS.Project.root().name())
    unit_name = Path(spec_file).stem

    logger.log(f"test2prove.run: spec={spec_loc}")
    logger.log(f"test2prove.run: spec_file={spec_file}, spec_line={spec_line}")
    logger.log(f"test2prove.run: project_name={project_name}")
    logger.log(f"test2prove.run: unit_name={unit_name}")

    # Get the hash that GNATtest will use
    try:
        hash_value = get_gnattest_hash(
            project_name,
            os.path.join("src", spec_file),
            spec_line,
        )
    except (ExternalProcessError, ValueError) as e:
        print_error(f"{e}")
        return

    # Find the artifacts directory
    found = False
    for f in GPS.Project.root().object_dirs(recursive=False):
        if os.path.isdir(os.path.join(f, "gnattest")):
            artifacts_dir = f
            found = True
            break

    if not found:
        print_error("Could not find 'gnattest' dir.")
        return

    config = Config(
        artifacts_dir=artifacts_dir,
        project_name=project_name,
        unit_name=unit_name,
        hash_value=hash_value,
        spec_file=spec_file,
        spec_line=spec_line,
        check_file=check_file,
        check_line=check_line,
        check_col=check_col,
    )

    # Generate test harness and initial test cases for the enclosing subprogram.

    print_info("Calling gnattest to generate harness and initial test cases ...")

    try:
        run_gnattest(
            config.spec_file,
            config.spec_line,
            GNATTEST_TARGET2 if use_fuzzer else GNATTEST_TARGET1,
            force,
        )
    except UserAbort:
        print_info("Aborted by the user")
        return
    except ExternalProcessError as e:
        print_error(f"{e}")
        return

    if use_fuzzer:
        try:
            subp_id = _get_gnatfuzz_subprogram_id(config)

            print_newline()
            print_info("Calling gnatfuzz to find counterexample values ...")

            # Make a call to gnatfuzz and perform the followup asynchronously.

            _fuzz_and_continue(config, subp_id, force)

            # _extract_ce_candidates will be called asynchronously in
            # _on_fuzzing_exit, so we just return here.

            return
        except Exception as e:
            print_error(f"{e}")
            return

    # If not using fuzzer, extract candidates synchronously
    _extract_ce_candidates(config)
