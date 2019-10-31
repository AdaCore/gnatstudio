"""
Check the behavior of "commit all staged files" action.
If there is staged files then it should only commit them.
If there is no staged files then it should staged all the modified files and
commit them.
"""

import GPS
from gs_utils.internal.utils import *
import gs_utils.internal.dialogs as dialogs


@run_test_driver
def test():
    view = Commits()
    yield view.open_and_yield()
    yield wait_idle()

    gps_assert(view.dump(),
               [('Modified',),
                ('Staged',),
                ('Untracked',),
                [('untracked.adb',)]],
               "No files modified at startup")

    # Modify and save files => update the Commits view Modified section
    for name in ["a.adb", "b.adb", "c.adb", "untracked.adb"]:
        buf = GPS.EditorBuffer.get(GPS.File(name))
        buf.insert(buf.at(1, 1), "--  This is a comment\n")
        buf.save(interactive=False)
        yield wait_tasks()
    gps_assert(view.dump(),
               [('Modified',),
                [('a.adb',), ('b.adb',), ('c.adb',), ],
                ('Staged',),
                ('Untracked',),
                [('untracked.adb',)]],
               "Files should be modified in editors")

    # Staged a file => verify it was moved in the Commits view
    yield view.stage_via_name(names=["b.adb"])
    gps_assert(view.dump(),
               [('Modified',),
                [('a.adb',), ('c.adb',), ],
                ('Staged',),
                [('b.adb',), ],
                ('Untracked',),
                [('untracked.adb',)]],
               "The file should be staged")

    # Commit the stage file => it should not affect the modified files
    view.set_message("Commit staged files")
    yield view.commit_staged()
    yield wait_idle()
    gps_assert(view.dump(),
               [('Modified',),
                [('a.adb',), ('c.adb',), ],
                ('Staged',),
                ('Untracked',),
                [('untracked.adb',)]],
               "Only the staged file should be commited")

    # When we don't have staged file commiting should stage all the
    # modified files
    view.set_message("Commit all the modified files")
    yield view.commit_staged()

    # Timeout necessary because we are executing 2 git commands:
    # "git add" and then "git commit"
    while view.dump() != [('Modified',),
                          ('Staged',),
                          ('Untracked',),
                          [('untracked.adb',)]]:
        yield timeout(500)

    # No file staged and no file modified => the action should not
    # raise an error.
    view.set_message("Commit nothing")
    yield view.commit_staged()
    yield wait_idle()
    gps_assert(view.dump(),
               [('Modified',),
                ('Staged',),
                ('Untracked',),
                [('untracked.adb',)]],
               "Nothing should be done")
