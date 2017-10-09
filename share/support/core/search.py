"""
This package extends the GPS.Search class with a number of high-level
python constructs. See the documentation of GPS.Search for more information.
"""

import GPS

GPS.Search.FUZZY = 1
GPS.Search.SUBSTRINGS = 2
GPS.Search.REGEXP = 4
GPS.Search.CASE_SENSITIVE = 8
GPS.Search.WHOLE_WORD = 16

GPS.Search.FILE_NAMES = "File names"
GPS.Search.ACTIONS = "Actions"
GPS.Search.BUILDS = "Build"
GPS.Search.OPENED = "Opened"
GPS.Search.ENTITIES = "Entities"
GPS.Search.SOURCES = "Sources"
GPS.Search.BOOKMARKS = "Bookmarks"
GPS.Search.PREFERENCES = "Preferences"
GPS.Search.PLUGINS = "Plugins"


def __iter__(self):
    return self


def next(self):
    """
    See documentation in the GPS user's guide.
    """

    while True:
        (has_next, result) = self.get()
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
