# -*- coding: utf-8 -*-
# Stress-tester for editor synchronization with LSP servers
#
# Launch with
#   gnatstudio --load=python:editor_sync.py <a_file>
#
# The file should be a file of the language for which you wish to test
# the server.

import GPS
from gs_utils.internal.utils import run_test_driver, wait_tasks, timeout
from workflows import task_workflow
import random


N_OPERATIONS = 1000
RANDOM_SNIPPET_SIZE_RANGE = 100  # range size of random snippets
RANDOM_TEXT = ["a", "b", "é", ";", " ", "\n"]


def delete_random(g):
    biggest_offset = g.get_end_iter().get_offset()
    o1 = random.randrange(biggest_offset + 1)
    o2 = random.randrange(biggest_offset + 1)

    g.delete(g.get_iter_at_offset(o1), g.get_iter_at_offset(o2))


def insert_random(g):
    biggest_offset = g.get_end_iter().get_offset()
    o = random.randrange(biggest_offset + 1)
    g.insert(
        g.get_iter_at_offset(o),
        "".join(
            [
                random.choice(RANDOM_TEXT)
                for j in range(random.randrange(RANDOM_SNIPPET_SIZE_RANGE))
            ]
        ),
    )


@run_test_driver
def driver():
    buf = GPS.EditorBuffer.get()
    g = buf.gtk_text_buffer()

    def wf(task):
        """The task that does random edits on the buffer"""
        choices = [insert_random, delete_random]

        for j in range(N_OPERATIONS):
            # Do a random operation
            fun = random.choice(choices)
            fun(g)
            # timeout from time to time so the display and progress bar can
            # refresh
            if j % 10 == 0:
                yield timeout(50)
                task.set_progress(j, N_OPERATIONS)

    task_workflow("gremlins", wf)
    yield wait_tasks()
