"""Fixtures for external-editor tests (Unix executable scripts).

Only the fake editor's child processes are waited for during cleanup: doing
that earlier would hide a missing reaper in External_Editor_Module.
"""

import json
import os
from pathlib import Path
import shutil
import signal
import tempfile
import time

import GPS
from gs_utils.internal.utils import gps_assert, timeout


ACTION = "edit with external editor"

# Each fake executable is self-contained and uses an absolute Python shebang,
# so an isolated PATH cannot accidentally invoke an installed editor.
EDITOR_SCRIPT = """
import json
import os
from pathlib import Path
import sys
import time

root = Path(CONFIG["root"])
name = Path(sys.argv[0]).name
record = root / (name + "-" + str(os.getpid()) + ".json")
temporary = record.with_suffix(".tmp")
temporary.write_text(json.dumps({
    "pid": os.getpid(),
    "argv": sys.argv[1:],
    "environment": dict(os.environ),
}))
temporary.replace(record)

if CONFIG["hold"]:
    # Bound the lifetime even if the IDE crashes before fixture cleanup.
    deadline = time.monotonic() + 30
    while not (root / (name + ".release")).exists():
        if time.monotonic() >= deadline:
            sys.exit(2)
        time.sleep(0.02)

(root / (name + ".finished")).touch()
sys.exit(0)
"""


def wait_for(predicate, message, seconds=5):
    """Poll without blocking GTK, and report a bounded assertion failure."""
    deadline = time.monotonic() + seconds
    while not predicate() and time.monotonic() < deadline:
        yield timeout(50)
    return gps_assert(bool(predicate()), True, message)


class FakeEditors:
    def __init__(self):
        self.root = Path(tempfile.mkdtemp(prefix="external-editor-", dir=os.getcwd()))
        self.bin = self.root / "bin"
        self.bin.mkdir()
        # sys.executable can name the IDE when Python is embedded.
        self.python = shutil.which("python3") or shutil.which("python")
        if not self.python:
            raise RuntimeError("External-editor fixtures require Python on PATH")
        self.environment = {}
        self.executables = []

    def __enter__(self):
        return self

    def __exit__(self, *exc):
        for name in self.executables:
            self.release(name)
        # Clean up only children positively identified by their own records.
        for name in self.executables:
            for record in self.records(name):
                pid = record["pid"]
                try:
                    child, _ = os.waitpid(pid, os.WNOHANG)
                    if child == 0:
                        os.kill(pid, signal.SIGTERM)
                        os.waitpid(pid, 0)
                except ChildProcessError:
                    pass  # Already reaped by GNAT Studio.
                except ProcessLookupError:
                    pass
        for name, value in self.environment.items():
            GPS.setenv(name, value)
        shutil.rmtree(self.root)

    def executable(self, name, hold=False):
        config = {"root": str(self.root), "hold": hold}
        path = self.bin / name
        path.write_text(
            "#!" + self.python + "\nCONFIG = " + repr(config) + "\n" + EDITOR_SCRIPT
        )
        path.chmod(0o755)
        self.executables.append(name)
        return str(path)

    def setenv(self, name, value):
        if name not in self.environment:
            self.environment[name] = GPS.getenv(name)
        GPS.setenv(name, value)

    def isolate_path(self):
        self.setenv("PATH", str(self.bin))

    def select(self, client, command=None):
        GPS.Preference("External-Editor-Default-Editor").set(client)
        if command is not None:
            GPS.Preference("External-Editor-Custom-Command").set(command)

    def open_file(self, name="source with spaces.txt", line=3, column=4):
        path = self.root / name
        # The action uses the entity's starting column from the context.
        path.write_text((" " * (column - 1) + "abcdefghi\n") * 10)
        buffer = GPS.EditorBuffer.get(GPS.File(str(path)))
        buffer.current_view().goto(buffer.at(line, column))
        return str(path)

    def records(self, name):
        return [
            json.loads(path.read_text()) for path in self.root.glob(name + "-*.json")
        ]

    def release(self, name):
        (self.root / (name + ".release")).touch()

    def finished(self, name):
        return (self.root / (name + ".finished")).exists()

    def invoke(self):
        # This return value reports filter matching, not Command_Return_Type.
        gps_assert(GPS.Action(ACTION).can_execute(), True, "Missing file context")
        GPS.execute_action(ACTION)
