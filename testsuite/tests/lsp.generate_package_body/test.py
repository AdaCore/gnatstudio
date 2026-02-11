"""
Test the "Generate Package Body" code action for an Ada package spec file.
This test verifies that when the code action is executed,
a corresponding body file is created with the expected structure,
and that both the spec and body files are opened in GNAT Studio.
"""

import GPS
from gs_utils.internal.utils import (
    run_test_driver,
    wait_language_server,
    gps_assert,
    wait_idle,
    wait_tasks,
)
import os


@run_test_driver
def driver():
    # Open the package spec file
    spec_file = GPS.File("my_package.ads")
    b = GPS.EditorBuffer.get(spec_file)
    v = b.current_view()

    # Place cursor on the package name to trigger "Generate Package" code action
    v.goto(b.at(1, 10))

    # Wait for the language server to respond with codeAction
    yield wait_language_server("textDocument/codeAction", "Ada")

    # Verify that we have code action messages
    m = GPS.Message.list(category="_internal_code_actions")
    gps_assert(len(m) > 0, True, "Should have at least one code action available")

    # Find the "Generate Package" code action
    generate_package_action = None
    for msg in m:
        if "Generate Package" in msg.get_text():
            generate_package_action = msg
            break

    gps_assert(
        generate_package_action is not None,
        True,
        "Should have 'Generate Package' code action",
    )

    # Get the body file path - it should not exist yet
    body_file_path = os.path.join(
        GPS.Project.root().file().directory(), "my_package.adb"
    )

    gps_assert(
        os.path.exists(body_file_path),
        False,
        "Body file should not exist before executing the code action",
    )

    # Execute the code action
    generate_package_action.execute_action()

    # Wait for the language server to execute the command
    yield wait_language_server("workspace/executeCommand", "Ada")
    yield wait_tasks()
    yield wait_idle()

    # Verify that the body file was created
    gps_assert(
        os.path.exists(body_file_path),
        True,
        "Body file should exist after executing the code action",
    )

    # Verify that the body file was opened in an editor
    body_file = GPS.File("my_package.adb")
    body_buffer = GPS.EditorBuffer.get(body_file, open=False, force=False)

    gps_assert(body_buffer is not None, True, "Body file should be opened in an editor")

    # Verify the body file has the expected structure
    body_content = body_buffer.get_chars()
    expected_declarations = [
        "package My_Package" "procedure Do_Something",
        "function Get_Value return Integer",
    ]
    gps_assert(
        any(declaration in body_content for declaration in expected_declarations),
        True,
        "Body file does not contain expected procedure or function declarations: \n"
        + body_content,
    )

    # Verify that both editors are open (spec and body)
    editors = [child.name(short=True) for child in GPS.MDI.children()]
    gps_assert("my_package.ads" in editors, True, "Spec editor should still be open")

    gps_assert("my_package.adb" in editors, True, "Body editor should be open")
