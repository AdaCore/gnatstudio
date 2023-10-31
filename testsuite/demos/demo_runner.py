"""
Python script that runs the actual demo.
"""

import argparse
import os
import shutil
import subprocess
import yaml

argParser = argparse.ArgumentParser()
argParser.add_argument(
    "-d", "--demo-dir", help="Path to the demo's directory", required=True
)

args = argParser.parse_args()

demo_dir = args.demo_dir
working_dir = directory = os.getcwd()
settings_dir = os.path.join(working_dir, ".gnatstudio")

if os.path.exists(settings_dir):
    shutil.rmtree(settings_dir)

gs_env = os.environ.copy()
gs_env["GNATSTUDIO_HOME"] = working_dir

with open(os.path.join(demo_dir, "demo.yaml"), "r") as file:
    demo_info = yaml.safe_load(file)
    project_file = os.path.join(demo_dir, demo_info["project"])

cmd = [
    "gnatstudio",
    "-P",
    project_file,
    "--load=python:%s" % (os.path.join(demo_dir, "test.py")),
]
print("Launching GNAT Studio: %s" % " ".join(cmd))
subprocess.Popen(cmd, env=gs_env)
