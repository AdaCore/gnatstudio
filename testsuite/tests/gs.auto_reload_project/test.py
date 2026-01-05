"""
Test the preference that controls automatic project reloads when project files
that belong to the loaded project tree are modified by the user.

This test checks that:
1. Modifying a project file that does not belong to the loaded tree: no reload
2. Modifying a project file that belongs to the project tree: reload happens
3. Preference disabled + modifying a project file in the tree: no reload
"""

import GPS
from gs_utils.internal.utils import (
    run_test_driver,
    gps_assert,
    wait_tasks,
)


@run_test_driver
def test_driver():
    """Test the Auto_Reload_Project preference"""

    # Counter to track project_view_changed hook calls
    project_reloaded = False

    def on_project_view_changed(hook_name):
        """Callback for project_view_changed hook"""
        nonlocal project_reloaded
        project_reloaded = True

    # Register our hook callback
    GPS.Hook("project_view_changed").add(on_project_view_changed)

    yield wait_tasks()

    # Test 1: Modifying a project file NOT in the loaded tree - no reload
    # ======================================================================
    GPS.Preference("Prj-Editor-Auto-Reload-Project").set(True)
    project_reloaded = False

    # Modify the external project file (not part of the loaded tree)
    external_file = GPS.File("external.gpr")
    external_buffer = GPS.EditorBuffer.get(external_file)

    # Make a simple modification to the file
    external_buffer.insert(external_buffer.end_of_buffer(), "\n-- Test modification\n")

    # Save the file - this should NOT trigger a reload
    external_buffer.save()
    yield wait_tasks()

    gps_assert(
        project_reloaded,
        False,
        "Test 1 failed: project_view_changed hook should NOT be called "
        "when modifying a project file outside the loaded tree",
    )

    # Revert the file to clean state
    external_buffer.undo()
    external_buffer.save()
    yield wait_tasks()

    # Test 2: Modifying a project file IN the loaded tree - reload happens
    # ======================================================================
    project_reloaded = False

    # Modify the main project file (part of the loaded tree)
    main_file = GPS.File("default.gpr")
    main_buffer = GPS.EditorBuffer.get(main_file)

    # Make a simple modification to the file
    main_buffer.insert(main_buffer.end_of_buffer(), "\n-- Test modification\n")

    # Save the file - this SHOULD trigger a reload
    main_buffer.save()
    yield wait_tasks()

    gps_assert(
        project_reloaded,
        True,
        "Test 2 failed: project_view_changed hook SHOULD be called "
        "when modifying a project file in the loaded tree",
    )

    # Revert the file to clean state
    main_buffer.undo()
    main_buffer.save()
    yield wait_tasks()

    # Test 3: Preference disabled - no reload even for files in the tree
    # ===================================================================
    GPS.Preference("Prj-Editor-Auto-Reload-Project").set(False)
    project_reloaded = False

    # Modify the main project file again
    main_buffer.insert(main_buffer.end_of_buffer(), "\n-- Test modification\n")

    # Save the file - this should NOT trigger a reload when preference is off
    main_buffer.save()
    yield wait_tasks()

    gps_assert(
        project_reloaded,
        False,
        "Test 3 failed: project_view_changed hook should NOT be called "
        "when Auto-Project-Reload preference is disabled",
    )

    # Revert the file to clean state
    main_buffer.undo()
    main_buffer.save()
    yield wait_tasks()
