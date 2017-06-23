"""This plugin adds a menu for computing the dependencies between two files

When you modify some source files, it often happens that the compiler will
decide to recompile other files that depended on it. Finding out why there is
such a dependency is not always obvious, and this plugin will help you in
that respect.

Once it has been activated, select the new menu
   /Navigate/Show File Dependency Path...
and in the dialog that appears enter the two source files you are interested
in. This will then list in the console why the first file depends on the
second (for instance "file1" depends on "file2", which depends on "file3")
"""

#############################################################################
# No user customization below this line
#############################################################################

import GPS
import os.path
from gps_utils import interactive


def internal_dependency_path(from_file, to_file, include_implicit):
    # We do the computation starting from to_file, since it is more efficient
    # to compute "imported" files than "importing files". Since we want to
    # compute the path itself, do not do a recursive search.
    deps = dict()

    # List of files to analyze. This is a list of tuples, the first element of
    # which is the name of the file to analyze, and the second is the name of
    # the parent that put it in the list
    to_analyze = [(from_file, None)]

    while len(to_analyze) != 0:
        (file, because_of) = to_analyze.pop()
        imports = file.imports(include_implicit=include_implicit,
                               include_system=False)

        # imports does not list the dependency from body to spec, so we add it
        # explicitly if from_file is a body.

        ext = os.path.splitext(from_file.path)
        if ext[1] == ".adb" or (ext[1] == ".ada" and ext[0][-2:] == ".2"):
            imports.append(from_file.other_file())

        deps[file] = because_of
        if file == to_file:
            break

        for f in imports:
            if f and f not in deps:
                to_analyze.append((f, file))

    target = to_file
    result = ""
    targets = []

    while target:
        targets.append(target)
        result = " -> " + target.path + "\n" + result
        if target not in deps:
            result = "No dependency between these two files"
            break
        target = deps[target]
    return (result, targets)


def dependency_path(from_file, to_file, fill_location=False, title=""):
    """Shows why modifying to_file implies that from_file needs to be
       recompiled. This information is computed from the cross-references
       database, and requires your application to have been compiled
       properly. This function does not attempt to compute the shortest
       dependency path, and just returns the first one it finds.
       FROM_FILE and TO_FILE must be instances of GPS.File.
       If FILL_LOCATION is True, then the locations view will also be
       filled."""

    if not isinstance(from_file, GPS.File):
        from_file = GPS.File(from_file)
    if not isinstance(to_file, GPS.File):
        to_file = GPS.File(to_file)

    if from_file == to_file:
        return "Same file"

    # First, try without implicit dependencies, this gives better results
    # in general, and then fallback to implicit deps if needed.

    (result, targets) = internal_dependency_path(from_file, to_file,
                                                 include_implicit=False)

    if result == "No dependency between these two files":
        (result, targets) = internal_dependency_path(from_file, to_file,
                                                     include_implicit=True)

    if fill_location and result != "No dependency between these two files":
        target = targets.pop()

        # Fill the locations view with the result
        while len(targets) != 0:
            prev_target = target
            target = targets.pop()

            # Assume simple naming schemes:
            #   - parent-child.ads -> parent.child
            #   - parent.child.1.ada -> parent.child
            # ??? Would be good to have a file_to_unit API instead
            unit = os.path.splitext(os.path.basename(target.path))[0]

            if len(unit) > 2 and (unit[-2:] == ".1" or unit[-2:] == ".2"):
                unit = unit[0:len(unit) - 2]

            unit = unit.split('-')[-1].split('.')[-1]

            # Find the 'with <unit>' clause in prev_target and fill the
            # location view
            for e in prev_target.entities(local=False):
                if e.category() == "package/namespace" \
                        and e.name().lower() == unit \
                        and e.declaration().file() == target:
                    refs = e.references(in_file=prev_target)

                    if len(refs) > 1:
                        r = refs[1]
                        GPS.Locations.add(category=title,
                                          file=prev_target,
                                          line=r.line(),
                                          column=r.column(),
                                          message="with " + unit,
                                          highlight="",
                                          length=0)
                        added = True
                        break
        if added:
            GPS.MDI.get("Locations").raise_window()
    return result


def print_dependency_path(from_file, to_file):
    title = "Dependencies from " + os.path.basename(from_file.path) + \
        " to " + os.path.basename(to_file.path)
    result = dependency_path(from_file, to_file, True, title)
    GPS.Console().write(title + "\n" + result + "\n")


@interactive(name='explain file dependency',
             menu='/Navigate/Show File Dependency Path...')
def interactive_dependency_path():
    """
    Explains one of the reasons why a file depends on another one (through
    a chain of with clauses or #include statements).
    """

    try:
        (file1, file2) = GPS.MDI.input_dialog("Show file dependency path",
                                              "From File", "To File")
    except:
        return

    print_dependency_path(GPS.File(file1), GPS.File(file2))
