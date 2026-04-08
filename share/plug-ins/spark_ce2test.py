"""
This plugin provides the functionality to generate a test harness with GNATtest
using counterexample values provided by GNATprove for a chosen SPARK check.

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
    ContextError,
    get_context_data,
    logger,
    ExternalProcessError,
    print_error,
    print_info,
    print_warning,
)
from workflows.promises import ProcessWrapper, TargetWrapper

# The following target names must be unique. See similar constants in
# spark_cegen.py.

# Note: Pluralization is intentionally in parentheses because we need one test
# case with JSON data for the counterexample, but GNATtest also generates the
# default test case. Also, the user can amend the number in the command.
GNATTEST_TARGET1 = "Generate harness and initial test case(s)"
GNATTEST_TARGET2 = "Update test case(s)"


def _get_values_from_spark_json(
    JSON_file, source_file, source_line, source_col, message
):
    """Retrieve counterexample values from a .spark file."""

    source_file = os.path.basename(source_file)

    logger.log(
        f"Searching for counterexample for {message!r}"
        f" at {source_file}:{str(source_line)}:{str(source_col)}"
        f" in {JSON_file!r}"
    )

    try:
        with open(JSON_file, "r") as f:
            data = json.load(f)

            proofs = data.get("proof", {})
            for proof in proofs:

                json_check_file = proof["check_file"]
                json_check_line = proof["check_line"]
                json_check_col = proof["check_col"]
                json_message_text = proof["message"]["text"]

                if (
                    json_check_file == source_file
                    and str(json_check_line) == str(source_line)
                    and str(json_check_col) == str(source_col)
                    and str(json_message_text).startswith(str(message))
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


def get_gnattest_hash(project_path, filename, line):
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
        f"-P{project_path}",
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


def run(context, check_message, force=False):
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

    :param context: GPS context for the check.
    :param check_message: The top-level part of the GNATprove's check message.
        It is used to match the entry in the .spark file with the right check
        when there are multiple checks for the same line and column.
    :param force: Boolean flag passed to GPS.BuildTarget. If True, all dialogs
        are suppressed and the processes are launched directly.
    """

    # Remove any previous messages related to test generation

    GPS.Locations.remove_category(GNATPROVE_TEST_GEN_CATEGORY)

    print_info("Starting to generate executable test...")

    # Extract check details

    try:
        config = get_context_data(context)
    except ContextError as e:
        print_error(f"{e}")
        return

    # Find the .spark file for the Ada unit that defines the enclosing subprogram

    spark_filename = os.path.join(
        config.artifacts_dir, "gnatprove", f"{config.unit_name}.spark"
    )
    if not os.path.exists(spark_filename):
        print_error(f"File '{spark_filename}' not found")
        return
    logger.log(f"Found .spark file {spark_filename}")

    # Retrieve the relevant counterexample from the .spark file

    ce_values = _get_values_from_spark_json(
        spark_filename,
        config.check_file,
        config.check_line,
        config.check_col,
        check_message,
    )

    if not ce_values:
        print_warning(
            f"No counterexample found for {check_message!r} at"
            f" {config.check_file}:{config.check_line}. Aborting"
        )
        return

    # Get the hash that GNATtest will use for the enclosing subprogram

    try:
        hash_value = yield get_gnattest_hash(
            config.project_path,
            os.path.join("src", config.spec_file),
            config.spec_line,
        )
    except (ExternalProcessError, ValueError) as e:
        print_error(f"{e}")
        return

    # Generate test harness and initial test cases for the enclosing subprogram.

    print_info("Calling gnattest to generate harness and initial test cases ...")

    try:
        yield run_gnattest(config.spec_file, config.spec_line, GNATTEST_TARGET1, force)
    except ExternalProcessError as e:
        print_error(f"{e}")
        return

    json_file = os.path.join(
        config.artifacts_dir,
        "gnattest",
        "tests",
        "JSON_Tests",
        f"{config.unit_name}.json",
    )

    # Update the test values in the GNATtest JSON file with the GNATprove
    # counterexample

    if not os.path.isfile(json_file):
        print_error(f"GNATtest generated JSON file not found: {json_file}")
        json_files = glob.glob(
            os.path.join(config.artifacts_dir, "**", "*.json"), recursive=True
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
        yield run_gnattest(config.spec_file, config.spec_line, GNATTEST_TARGET2, force)
    except ExternalProcessError as e:
        print_error(f"{e}")
        return

    harness_path = os.path.join(
        config.artifacts_dir, "gnattest", "harness", "test_driver.gpr"
    )

    print_info(f"Test harness has been generated and can be found at {harness_path}")

    GPS.Message(
        GNATPROVE_TEST_GEN_CATEGORY,
        GPS.File(config.check_file),
        config.check_line,
        config.check_col,
        f"Test harness has been generated and can be found at {harness_path}",
    )
