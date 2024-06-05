# -*- coding: utf-8 -*-
# Stress-tester for sending invalid code to the language server
# and verify that requests work otherwise.
#
# Launch with
#   gnatstudio --load=python:als_request_stress.py
#
# The file should be a file of the language for which you wish to test
# the server.
# From there, launch the interactive action "fuzz" to start testing,
# and launch the interactive action again to stop testing. (You will need
# to define a key shortcut for this action).
#
# The test will interrupt itself if it detects that a query that used
# to work with valid code no longer works with the same valid code.
#
# Note: at the moment it works only for Ada sources.

import GPS
import libadalang as lal
from gs_utils import interactive
from gs_utils.internal.utils import wait_tasks
from workflows import task_workflow
import random


N_OPERATIONS = 1000
# Max number of loops to do. Use a negative number for infinite looping.


def delete_random(g):
    biggest_offset = g.get_end_iter().get_offset()
    o1 = random.randrange(biggest_offset + 1)
    o2 = random.randrange(biggest_offset + 1)

    g.delete(g.get_iter_at_offset(o1), g.get_iter_at_offset(o2))


def insert_random(g):
    biggest_offset = g.get_end_iter().get_offset()
    o = random.randrange(biggest_offset + 1)
    text = "".join(
        [random.choice("abcdefghijklmnopqrstuvwxyz;()-\"'\n") for j in range(100)]
    )
    g.insert(g.get_iter_at_offset(o), text)


IN_PROGRESS = False


@interactive("Editor", "Source editor", name="fuzz")
def driver():
    # interrupt the action if it's being run again
    global IN_PROGRESS
    if IN_PROGRESS:
        IN_PROGRESS = False
        return

    IN_PROGRESS = True
    buf = GPS.EditorBuffer.get()
    lang = buf.file().language()
    als = GPS.LanguageServer.get_by_file(buf.file())
    ada_sources = [f for f in GPS.Project.root().sources(True) if f.language() == lang]

    def random_ada_source():
        return random.choice(ada_sources)

    def random_query(buf):
        unit = buf.get_analysis_unit()
        identifiers = unit.root.findall(lambda x: x.is_a(lal.Identifier))
        identifier = random.choice(identifiers)
        line = identifier.sloc_range.start.line
        column = identifier.sloc_range.start.column

        method = "textDocument/definition"
        params = {
            "textDocument": {"uri": "file://{}".format(buf.file().name())},
            "position": {"line": line - 1, "character": column - 1},
        }

        return (method, params)

    def wf(task):
        """The task that does random edits on the buffer"""
        choices = [insert_random]

        good_queries = []
        # contains (method, params, result)

        # Start
        counter = 0
        while counter != N_OPERATIONS:
            counter += 1

            # allow breaking the workflow
            global IN_PROGRESS
            if not IN_PROGRESS:
                break

            # Select a source at random and open a buffer for it
            f = random_ada_source()

            buf = GPS.EditorBuffer.get(f)
            g = buf.gtk_text_buffer()

            # Generate a random query on the buffer
            method, params = random_query(buf)

            # Execute this random query and get the result
            result = yield als.request_promise(method, params)
            orig_result = str(result)

            # Store the result in the "good queries" that we've done
            good_queries.append((method, params, orig_result))

            # Now do a random edit
            fun = random.choice(choices)
            fun(g)

            # Generate another random query on the buffer, this time
            # when the buffer is (most probably) invalid, just to throw LAL off
            bad_method, bad_params = random_query(buf)
            yield als.request_promise(bad_method, bad_params)

            # now undo the operation
            buf.undo()

            # Verify that the initial request still works!
            result = yield als.request_promise(method, params)
            new_result = str(result)

            if new_result != orig_result:
                GPS.MDI.dialog("{}\n  /=  \n{}".format(new_result, orig_result))
                break

            # TODO: execute a past random request

    task_workflow("gremlins", wf)
    yield wait_tasks()
