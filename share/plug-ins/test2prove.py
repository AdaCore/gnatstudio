# This script calls gnattest on an ADA subprogram or project to generate
# test cases then feed them to gnatprove for it to try to use them as
# CE values.

import os
import sys
import json
import argparse
import subprocess
import os_utils
import subprocess

import GPS


# Generate test values with gnattest
def run_gnattest(project_name, filename, line, nb_cases):
    return GPS.Process(
        [
            "gnattest",
            f"-P{project_name}",
            filename,
            f"--gen-test-subprograms={filename}:{str(line)}",
            "--gen-test-vectors",
            f"--gen-test-num={nb_cases}",
        ]
    )


# Run gnatfuzz with user-handed arguments
def gnatfuzz(subcommand, subsubcommand, project_name, args):
    with open("log", "w+") as redirected_output:
        return GPS.Process(
            ["gnatfuzz", subcommand, subsubcommand, f"-P{project_name}.gpr"]
            + args
            + ["--disable-styled-output"],
            stdout=redirected_output,
        )


# Generate 'better' test values with gnatfuzz
def run_gnatfuzz(project_name, sub_id):
    return gnatfuzz(
        "fuzz-everything",
        "test",
        project_name,
        [f"--subprogram-ids-to-test={sub_id}"],
    )


# Retreive a subprogram's id according to gnatfuzz
def get_subprogram_id(project_name, filename, line, artifacts):
    proc = gnatfuzz("fuzz-everything", "inspect", project_name, [])

    proc.wait()

    json_path = os.path.join(artifacts, "gnatfuzz", "analyze.json")

    try:
        with open(json_path, "r") as f:
            data = json.load(f)

            subp = data.get("fuzzable_subprograms", {})
            for sub in subp:

                start_line = sub.get("start_line", {})
                source_filename = sub.get("source_filename", {})

                if (
                    str(start_line) == str(line)
                    and filename in source_filename
                ):
                    return sub.get("id", {})

            return None

    except FileNotFoundError:
        GPS.Console().write(f"Error: {json_path} not found.")
        return None
    except json.JSONDecodeError:
        GPS.Console().write(f"Error: Invalid JSON format in {json_path}.")
        return None
    except Exception as e:
        GPS.Console().write(f"An unexpected error occurred: {e}")
        return None


# Retreive the hash16 that gnattest uses to refer to a subprogram
def get_hash(project_name, filename, line):
    command = [
        "gnattest",
        f"-P{project_name}",
        f"--dump-subp-hash={filename}:{line}",
        "--quiet",
    ]

    proc = GPS.Process(command, capture_output=True, text=True)

    proc.wait()

    return proc.get_result().strip("\n").strip("\t")


# Extract relevant data from gnattest's json
def get_test_case_from_gnattest(JSON_file, my_hash):
    try:
        with open(JSON_file, "r") as f:
            return json.load(f).get(my_hash, {}).get("test_vectors", {})

        return None

    except FileNotFoundError:
        GPS.Console().write(f"Error: filename  {JSON_file}  not found.")
        return None
    except json.JSONDecodeError:
        GPS.Console().write(f"Error: Invalid JSON format in {JSON_file}.")
        return None
    except Exception as e:
        GPS.Console().write(f"An unexpected error occurred: {e}")
        return None


def parse_subp(s):
    parts = s.split(':')
    if len(parts) == 3 and parts[1].isdigit() and parts[2].isdigit():
        return parts[0], int(parts[1]), int(parts[2])
    return None, None, None


# peform the entire pipeline
def run(spec, subp, check_type, package_name, nb_tests, gnatfuzz_mode=False):
    spec_file, spec_line, spec_col = parse_subp(spec)

    subp_file, subp_line, subp_col = parse_subp(subp)

    project_name = str.lower(GPS.Project.root().name())
    
    project_path = GPS.Project.root().file().path

    hash_value = get_hash(project_name,
                          os.path.join("src", spec_file),
                          spec_line)

    GPS.Console().write(f"test2prove starting..." + os.linesep)

    found = False
    for f in GPS.Project.root().object_dirs(recursive=False):
        if os.path.isdir(os.path.join(f, "gnattest")):
            artifacts = f
            found = True
            break

    if not found:
        GPS.Console().write("could not find artifacts dir.")
        return

    if not hash_value:
        GPS.Console().write(f"Warning: Could not get hash for {spec_file}:{spec_line}. Skipping.")
        os.chdir(current_working_dir)
        return
    
    proc = run_gnattest(project_name, spec_file, spec_line, nb_tests)

    proc.wait()

    if gnatfuzz_mode:
        subp_id = get_subprogram_id(project_name, spec_file, spec_line, artifacts)

        if subp_id is None:
            GPS.Console().write(
                f"Subprogram ID not found for {filename_for_gnattest}:{line} "
                + "in gnatfuzz JSON."
            )
            return

        proc = run_gnatfuzz(project_name, subp_id)

        proc.wait()

    gnattest_json_path = os.path.join(
        artifacts,
        "gnattest",
        "tests",
        "JSON_Tests",
        f"{package_name}.json",
    )

    test_values = get_test_case_from_gnattest(gnattest_json_path, hash_value)

    if not test_values:
        GPS.Console().write(
            "Warning: No test values found for"
            + f"{spec_file}:{spec_line}."
        )
        return

    CE_values_path = os.path.join(
        artifacts,
        "gnatprove",
        f"{project_name}_{package_name}_{spec_line}.ce")

    with open(CE_values_path, "w") as f:
        json.dump(test_values, f)

    GPS.Console().write("Counter example candidates have been generated " +
                        f"and can be found at {CE_values_path}")

    GPS.Message("default",
                GPS.File(subp_file),
                subp_line,
                subp_col,
                f"Counterexample file available at {CE_values_path}");
