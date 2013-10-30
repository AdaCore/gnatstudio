import os
import os.path
import re
import subprocess
import sys

OTOOL_LINE_FILTER = re.compile("(.*\.dylib\.?.*)\s\(compatibility.*$")


class Resolver(object):
    """Class used to track and resolve an executable's dependencies"""

    inputs = []
    to_copy = {}
    ignore = []
    to_analyze = []

    def __init__(self, prefix, mains, library_path):
        """Takes a list of libraries or executable as input, and creates self.to_copy
        containing the dependencies"""
        self.prefix = prefix
        self.library_path = library_path
        self._resolve_library_dependencies(mains)

    @staticmethod
    def filterlines(lines):
        """Filters lines with regexp and returns the group 1 from matching lines"""
        for line in lines:
            match = OTOOL_LINE_FILTER.match(line)
            if match:
                yield match.group(1).strip().split()[0]

    def prefix_filter(self, line):
        if line in self.ignore:
            return False
        if line in self.to_copy.keys():
            return True

        if not os.path.isabs(line):
            return False

        for prefix in self.library_path:
            if line.startswith(prefix):
                return True

        if not line.startswith("/usr/lib") and not line.startswith("/System/Library") and not line.strip().split()[0].endswith("libgcc_s.1.dylib"):
            basename = os.path.basename(line)
            for prefix in self.library_path:
                tentative=os.path.join(prefix, "lib", basename)
                if os.path.isfile(tentative):
                    print "Warning, using %s instead of %s: may have been moved" % (tentative, line)
                    return True
            # Ignore the library from the OS, and raise a warning for the others
            print "!!! ERROR, library not available in any prefix: %s" % line
            self.ignore.append(line)

        return False

    def to_bundle(self, filename, prefix):
        """Returns the bundle's filename for library filename"""
        return filename.replace(prefix, self.prefix)

    def _get_deps(self, filename):
        """Retrieve the list of filename's dynamic library in use"""

        cmd = "xcrun otool -L %s" % filename
        with os.popen(cmd) as f:
            libs = Resolver.filterlines([line.strip() for line in f])
            libs = filter(self.prefix_filter, libs)
            return set(libs)

    def _add_deps(self, f_list):
        """Filters libraries based on their location"""

        for lib in f_list:
            libs = self._get_deps (lib)
            for lib in libs:
                if lib in self.to_copy.keys():
                    continue

                if lib in self.ignore:
                    continue

                found = False
                for prefix in self.library_path:
                    if lib.startswith(prefix):
                        # From authorized prefixes
                        self.to_copy[lib] = (lib, self.to_bundle(lib, prefix))
                        self.inputs.append(lib)
                        found = True
                        break
                if found:
                    continue

                # lib not found in prefixes, so it's been moved at some point
                basename = os.path.basename(lib)
                found = False
                for prefix in self.library_path:
                    tentative=os.path.join(prefix, "lib", basename)
                    if os.path.isfile(tentative):
                        self.to_copy[lib] = (tentative, self.to_bundle(tentative, prefix))
                        self.inputs.append(tentative)
                        found = True
                        break


    def to_relative(self, filename):
        """Rebase the library dependencies relative to filename location"""

        lines = self._get_deps(filename)

        for lib in lines:
            libname = os.path.basename(lib)
            cmd = "xcrun install_name_tool -change %s %s %s" % (lib, libname, filename)
            subprocess.call(cmd, shell=True)
        cmd = "xcrun install_name_tool -id %s %s" % (os.path.basename(filename), filename)
        subprocess.call(cmd, shell=True)

    def _resolve_library_dependencies(self, mains):
        """Recursively gather all library dependencies of executable mainpath"""

        self.inputs = mains[:]
        n_iterations = 0
        n_paths = -1

        for main in mains:
            do_copy = False
            # First check if we need to copy the files given as input
            for prefix in self.library_path:
                if main.startswith(prefix):
                    self.to_copy[main] = (main, main.replace(prefix, self.prefix))
                    do_copy = True
                    break
            if not do_copy:
                self.to_copy[main] = ("", main)

        # we iterate until we don't have any new library
        while n_paths != len(self.to_copy):
            n_paths = len(self.to_copy)

            # run otool on the input files
            self._add_deps(self.inputs[:])

            n_iterations += 1
            if n_iterations > 10:
                print "Too many tries to resolve library dependencies"
                sys.exit(1)

