"""
This package provides a high-level interface to the search facilities
used for the GPS omni-search.
This package is implemented on top of a few low-level functions exported
by GPS, which we do not recommend to use. Instead, using the class in this
package provides an easier to use interface.

Here are some examples on the use of this package::

    # Print all filenames matching "gkms"

    for n in Search.search(Search.FILE_NAMES, "gkms", Search.FUZZY):
        print n.short

    # Print all lines matching "Fo[od]" in project sources

    for n in Search.search(Search.SOURCES, "Fo[od]", Search.REGEXP):
       print n.full

It is also possible to create your own search context specific to your
company, and have them fully integrated into GPS's omni search.

    # Create our own search context for file names

    class Filenames_SearchContext(SearchContext):
        name = "filenames_from_python"

        def __init__(self):
                self.matches = []

        def set_pattern(self, pattern, flags=0, limit=None):
            self.matches = [
               f.name()
               for f in GPS.Project.root().sources(recursive=True)
               if pattern in f.name()]

        def __iter__(self):
            return iter(self.matches)

    Search_Provider.register(Filenames_Search_Provider)

    for n in SearchContext.search("filenames_from_python", "gps-"):
        print n.short

"""

import GPS

GPS.Search.FUZZY = 1
GPS.Search.SUBSTRINGS = 2
GPS.Search.REGEXP = 4
# The various types of search, similar to what GPS provides in its
# omni-search.

GPS.Search.CASE_SENSITIVE = 8
GPS.Search.WHOLE_WORD = 16
# Flags to configure the search, that can be combined with the above.

GPS.Search.FILE_NAMES = "File names"
GPS.Search.ACTIONS = "Actions"
GPS.Search.BUILDS = "Build"
GPS.Search.OPENED = "Opened"
GPS.Search.ENTITIES = "Entities"
GPS.Search.SOURCES = "Sources"
GPS.Search.BOOKMARKS = "Bookmarks"
# The various contexts in which a search can occur.

def __iter__(self):
    return self

def next(self):
    """
    See documentation in the GPS user's guide.
    """

    while True:
        (has_next, result) = self.next()
        if result:
            return result
        if not has_next:
            raise StopIteration


def search(context, pattern, flags=GPS.Search.SUBSTRINGS):
    """
    See documentation in the GPS user's guide.
    """
    s = GPS.Search.lookup(context)
    if s:
        s.set_pattern(pattern, flags)
    return s


GPS.Search.__iter__ = __iter__
GPS.Search.next = next
GPS.Search.search = staticmethod(search)

