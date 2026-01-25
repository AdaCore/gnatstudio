"""
This plugin provides the functionality to generate a test harness with GNATtest
using counterexamples values from GNATprove for a chosen SPARK check.

A prerequisite is a failed check for which GNATprove has found a
counterexample.
"""

import glob
import json
import os
import re

import GPS
from gnatprove import (
    GNATPROVE_TEST_GEN_CATEGORY,
    logger,
    ExternalProcessError,
    print_error,
    print_info,
    print_warning,
)
from workflows.promises import ProcessWrapper, TargetWrapper

# The following target names must be unique. See similar constants in
# spark_test2prove.py.

# Note: Pluralization is intentionally in parentheses because we need one test
# case with JSON data for the counterexample, but GNATtest also generates the
# default test case. Also, the user can amend the number in the command.
GNATTEST_TARGET1 = "(Step 1/2) Generate harness and initial test case(s)"
GNATTEST_TARGET2 = "(Step 2/2) Update test case(s)"


def _get_values_from_spark_json(
    JSON_file, source_file, source_line, source_col, vc_kind
):
    """Retrieve counterexample values from a .spark file."""

    source_file = os.path.basename(source_file)

    logger.log(
        f"Searching for counterexample for {vc_kind!r}"
        f" at {source_file}:{str(source_line)}:{str(source_col)}"
        f" in {JSON_file!r}"
    )

    try:
        with open(JSON_file, "r") as f:
            data = json.load(f)

            proofs = data.get("proof", {})
            for proof in proofs:

                json_check_file = proof.get("check_file")
                json_check_line = proof.get("check_line")
                json_check_col = proof.get("check_col")
                json_rule = proof.get("rule")

                if (
                    json_check_file == source_file
                    and str(json_check_line) == str(source_line)
                    and str(json_check_col) == str(source_col)
                    and str(json_rule) == str(vc_kind)
                ):

                    logger.log("Proof entry for the given check found")

                    cntexmp_value = proof.get("cntexmp_value", {})
                    inputs = cntexmp_value.get("inputs", {})
                    logger.log(f"Retrieved counterexample values {inputs}")
                    return inputs

        logger.log("No proof entry found for the given check!")
        return None

    except FileNotFoundError:
        print_error(f"File {JSON_file} not found.")
        return None
    except json.JSONDecodeError:
        print_error(f"Invalid JSON format in {JSON_file}.")
        return None
    except Exception as e:
        print_error(f"An unexpected error occurred: {e}")
        return None


def _replace_values_in_gnattest_json(json_file, subp_hash, new_values):
    """
    Update the last GNATtest test case for the given subprogram with the given
    new values

    We assume that at least one test case existed for the subprogram indicated
    by 'subp_hash'. That's because this function is supposed to be called after
    the step which generated at least one test case. However, in general there
    might have also others already existing before. The one we created is the
    last one. So, we'll modify that one and replace the values provided by
    GNATtest with 'new_values' originating from a SPARK counterexample.
    """

    try:
        with open(json_file, "r") as f:
            data = json.load(f)

            subprogram = data.get(subp_hash, {})

            if subprogram == {}:
                # This should never happen
                print_error(
                    f"Subprogram with hash '{subp_hash}' not found in {json_file}"
                )
                return False

            test_vectors = data.get(subp_hash, {}).get("test_vectors", {})

            assert test_vectors, (
                f"FATAL ERROR: 'test_vectors' for subprogram '{subp_hash}' is empty in"
                f" {json_file}"
            )

            test_vectors[-1]["origin"] = "GNATprove"

            param_vectors = test_vectors[-1].get("param_values", {})

            for new_value in new_values:
                name = new_value.get("name", {})
                val = new_value.get("value", {})

                for param in param_vectors:
                    if param.get("name", {}) == name:
                        param["value"] = val

        with open(json_file, "w") as f:
            json.dump(data, f, indent=4)
            return True

    except FileNotFoundError:
        print_error(f"Filename {json_file} not found.")
        return False
    except json.JSONDecodeError:
        print_error(f"Invalid JSON format in {json_file}.")
        return False
    except Exception as e:
        print_error(f"An unexpected error occurred: {e}")
        return False


def split_location(s):
    """Split location to (file, line, col)."""

    parts = s.split(":")
    if len(parts) == 3 and parts[1].isdigit() and parts[2].isdigit():
        return parts[0], int(parts[1]), int(parts[2])
    return None, None, None


def get_gnattest_hash(project_name, filename, line):
    """
    Retrieve the hash that GNATtest uses to refer to the subprogram enclosing
    the given source location. The call to GNATtest occurs in the background.
    Parameters cannot be tweaked by the user.

    Exits with ExternalProcessError when the call to gnattest fails and
    ValueError when the obtained value is empty or doesn't seem to be a valid
    hash.
    """

    command = [
        "gnattest",
        f"-P{project_name}.gpr",
        f"--dump-subp-hash={filename}:{line}",
        "--quiet",
    ]
    logger.log(f"Calling gnattest to generate hash: {' '.join(command)}")

    proc = ProcessWrapper(command)

    status, output = yield proc.wait_until_terminate(show_if_error=True)
    logger.log(f"gnattest output: {output}")
    logger.log(f"gnattest finished with result: {status}")

    if status != 0:
        print_error(output)
        raise ExternalProcessError(f"Call to gnattest failed with exit code {status}")

    hash_value = output.strip()
    logger.log(f"Obtained hash value '{hash_value}'")

    # Check that the hash value is a non-empty word. This catches the following
    # possible situations:
    # * Empty string   - The given subprogram not supported for unit testing
    #   (e.g., it is not included in the closure of main).
    # * Multiple words - Some other failure message.
    HASH_PATTERN = re.compile(r"^\S+$")
    if not HASH_PATTERN.match(hash_value):
        raise ValueError(
            f"Received unexpected gnattest hash value '{hash_value}' for the enclosing"
            f" subprogram at {filename}:{line}."
            "\nMake sure the subprogram is included in the program's closure and"
            " supported by GNATtest."
        )

    yield hash_value


def run_gnattest(filename, line, target, force):
    """
    Open a dialog to call GNATtest for generating/updating test harness and
    tests for the enclosing subprogram. Parameters can be tweaked by the user.

    :param filename: File containing the subprogram's spec.
    :param line: File containing the subprogram's spec.
    :param target: Name of the predefined build target.
    :param force: If True, the dialog is suppressed and the command is launched
        directly.

    Exits with ExternalProcessError when the call to gnattest fails.
    """

    runner = TargetWrapper(target)
    status = yield runner.wait_on_execute(
        extra_args=[f"--gen-test-subprograms={filename}:{str(line)}"], force=force
    )
    logger.log(f"run_gnattest: finished with status={status}")

    if status != 0:
        raise ExternalProcessError(f"Call to gnattest failed with exit code {status}")


def run(spec_loc, check_loc, vc_kind, force=False):
    """
    Generate an executable harness for the given check location

    The harness is generated in the following main steps:

        1. Read counterexample values from a .spark file.
        2. Determine the hash that gnattest will use for the enclosing
           subprogram.
        3. Generate harness and initial test data with gnattest.
        4. Update the test data in a gnattest JSON file.
        5. Call gnattest again to update the test data in the Ada test cases
           based on the updated JSON file.

    Note: This function needs to be called as a GNAT Studio workflow.

    :param spec_loc: Source code location of the spec.
    :param check_loc: Source code location of the given GNATprove check.
    :param vc_kind: VC kind of the given GNATprove check.
    :param force: Boolean flag passed to GPS.BuildTarget. If True, all dialogs
        are suppressed and the processes are launched directly.
    """

    # Remove any previous messages related to test generation

    GPS.Locations.remove_category(GNATPROVE_TEST_GEN_CATEGORY)

    print_info("Starting to generate executable test...")

    # Find the .spark file for the Ada unit that defines the enclosing subprogram

    spec_file, spec_line, spec_col = split_location(spec_loc)
    check_file, check_line, check_col = split_location(check_loc)

    project_name = str.lower(GPS.Project.root().name())
    unit_name = os.path.splitext(os.path.basename(spec_file))[0]

    logger.log(f"testgen.run: spec={spec_loc}")
    logger.log(f"testgen.run: subp={check_loc}")
    logger.log(f"testgen.run: spec_file={spec_file}, spec_line={spec_line}")
    logger.log(
        f"testgen.run: check_file={check_file}, check_line={check_line}"
        f", check_col={check_col}"
    )
    logger.log(f"testgen.run: project_name={project_name}")
    logger.log(f"testgen.run: unit_name={unit_name}")

    spark_filenames = []
    for base_dir in GPS.Project.root().object_dirs(recursive=False):
        spark_filenames.extend(
            glob.glob(os.path.join(base_dir, "gnatprove", f"{unit_name}.spark"))
        )

    if not spark_filenames:
        print_error(f"Could not find generated .spark file for unit {unit_name}.")
        return
    elif len(spark_filenames) > 1:
        print_error(
            f"Multiple .spark files found for the unit '{unit_name}':"
            + "\n"
            + "\n".join(spark_filenames)
            + "\nPlease clean the object directories and try again."
        )
        return

    spark_filename = spark_filenames[0]
    artifacts_dir = os.path.dirname(os.path.dirname(spark_filename))

    logger.log(f"Found .spark file {spark_filename}")

    # Retrieve the relevant counterexample from the .spark file

    ce_values = _get_values_from_spark_json(
        spark_filename,
        check_file,
        check_line,
        check_col,
        vc_kind,
    )

    if ce_values is None:
        print_warning(
            f"Couldn't get counterexample for {vc_kind} at {check_file}:{check_line}."
            " Aborting"
        )
        return

    # Get the hash that GNATtest will use for the enclosing subprogram

    try:
        hash_value = yield get_gnattest_hash(
            project_name, os.path.join("src", spec_file), spec_line
        )
    except (ExternalProcessError, ValueError) as e:
        print_error(f"{e}")
        return

    #  Generate test harness and initial test cases for the enclosing subprogram.

    print_info("Calling gnattest to generate harness and initial test cases ...")

    try:
        yield run_gnattest(spec_file, spec_line, GNATTEST_TARGET1, force)
    except ExternalProcessError as e:
        print_error(f"{e}")
        return

    json_file = os.path.join(
        artifacts_dir,
        "gnattest",
        "tests",
        "JSON_Tests",
        f"{unit_name}.json",
    )

    # Update the test values in the GNATtest JSON file with the GNATprove
    # counterexample

    if not os.path.isfile(json_file):
        print_error(f"GNATtest generated JSON file not found: {json_file}")
        json_files = []
        for base_dir in GPS.Project.root().object_dirs(recursive=False):
            json_files.extend(
                glob.glob(os.path.join(base_dir, "**", "*.json"), recursive=True)
            )
        logger.log("Found .json files in object directories:\n" + "\n".join(json_files))
        print_error("Aborting")
        return

    if not _replace_values_in_gnattest_json(
        json_file,
        hash_value,
        ce_values,
    ):
        print_error(
            "Inserting counterexample values into test harness failed. Aborting"
        )
        return

    # Run GNATtest again to update the test cases based on the modified JSON
    # file.

    print_info("Calling gnattest to update test case with counterexample values ...")

    try:
        yield run_gnattest(spec_file, spec_line, GNATTEST_TARGET2, force)
    except ExternalProcessError as e:
        print_error(f"{e}")
        return

    harness_path = os.path.join(artifacts_dir, "gnattest", "harness", "test_driver.gpr")

    print_info(f"Test harness has been generated and can be found at {harness_path}")

    GPS.Message(
        GNATPROVE_TEST_GEN_CATEGORY,
        GPS.File(check_file),
        check_line,
        check_col,
        f"Test harness has been generated and can be found at {harness_path}",
    )
