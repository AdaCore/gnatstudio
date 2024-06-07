""" count the clangd processes that are children of the process passed as first argument
"""

import os
import psutil
import sys


the_pid = int(sys.argv[1])
child_clangds = 0
for x in psutil.pids():
    try:
        p = psutil.Process(x)
        # count the processes that are named clangd[.exe] and are children
        # of this process
        if p.ppid() == the_pid and p.name().startswith("clangd"):
            child_clangds += 1
    except psutil.NoSuchProcess:
        pass

print(child_clangds)
