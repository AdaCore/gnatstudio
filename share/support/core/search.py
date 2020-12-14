"""
This package extends the GPS.Search class with a number of high-level
python constructs. See the documentation of GPS.Search for more information.
"""

import GPS
import time

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
GPS.Search.CURRENT_FILE_ENTITIES = "Entities for current file"
GPS.Search.SOURCES = "Sources"
GPS.Search.BOOKMARKS = "Bookmarks"
GPS.Search.PREFERENCES = "Preferences"
GPS.Search.PLUGINS = "Plugins"


def __iter__(self):
    return self


def next(self):
    """
    Iterating over search result
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
        while not s.is_result_ready:
            time.sleep(10)
    return s


GPS.Search.__iter__ = __iter__
GPS.Search.next = next
GPS.Search.search = staticmethod(search)
