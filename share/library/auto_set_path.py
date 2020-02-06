"""This plugin will automatically add to the PATH:
    - the path that contains GPS,
    - the path to nearby compilers
This is meant to be used in controlled installations of GPS+GNAT,
such as the GNAT Community edition, to allow people to access the
full toolchain by simply launching GPS, without having to set the
environment outside of GPS.
"""

# This plugin can also serve as an example of how to set the PATH.

import GPS
import os
import glob


def compute_paths():
    """Compute the list of paths that should be added to the PATH"""
    paths = []

    # system_dir is the directory that contains the GPS install
    parent = os.path.abspath(os.path.join(GPS.get_system_dir(), '..'))

    # As a first approximation, we are going to go up one level, and add
    # the PATH all that share a parent dir with the GPS install.
    # For instance this supports an architecture of the form
    #    C:\GNAT\2018\gps\bin
    #    C:\GNAT\2018\gnat\bin
    #    C:\GNAT\2018\gnat-cross-arm-elf\bin
    # or, for instance
    #    /opt/gnat-community/2018/bin
    #    /opt/gnat-community/2018-cross-arm-elf/bin

    for tentative in glob.glob(os.path.join(parent, '*')):
        tentative_bin = os.path.join(tentative, 'bin')
        if os.path.isdir(tentative_bin):
            paths.append(tentative_bin)

    paths.sort()
    return paths


def add_to_paths(paths):
    """Add the given list of paths to the PATH"""

    path = GPS.getenv('PATH')
    existing_paths = path.split(os.pathsep)

    for j in paths:
        # Right now we're adding at the end of the PATH - that's putting
        # the tools on the PATH for people that have not modified at all
        # the environment on their machine, while preserving the behavior
        # for people who have added GNAT tools to their environment.
        # Also, for extra safety, we don't re-add at the end of the PATH
        # the paths that are already present in the PATH.
        if j not in existing_paths:
            path += os.pathsep + j

    GPS.setenv('PATH', path)
    # Set the PATH in the Python environment as well, so that plugins can
    # see the same path.
    os.environ['PATH'] = path


# Note: as a special exception, we do not run this plugin in reaction to
# the "gps_started" signal, but before, so that the plugins can benefit from
# the setting of the PATH.
add_to_paths(compute_paths())
