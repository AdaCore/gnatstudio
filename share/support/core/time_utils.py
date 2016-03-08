"""
This script provides a few utilities for handling time
displaying.
"""

import time
import datetime


class TimeDisplay(object):

    @staticmethod
    def get_timestamp(t):
        """
        Return a timestamp label from ``t``.

        Use the :func:`time.time` function to retrieve the current time since
        the epoch in seconds and get a timestamp label from it, like in the
        following example:

        >>> import time
        >>> t = time.time() # get the current time
        >>> ts = TimeDisplay.get_timestamp(t)
        >>> print ts
        [2016-03-10 09:10:03]

        :param t: A float representing the time in seconds since the epoch.
        :return: A timestamp string
        """

        return datetime.datetime.fromtimestamp(t).strftime(
            '[%Y-%m-%d %H:%M:%S]')

    @staticmethod
    def get_elapsed(start, end):
        """
        Return a string representing the elapsed time between
        ``start`` and ``end``.

        Here is a simple example of how `start` and `end` can
        be computed using the :func:`time.time` function:

        >>> import time
        >>> start = time.time() # get the start time
        >>> very_long_process()
        >>> end = time.time() # get the end time
        >>> elapsed = TimeDisplay.get_elapsed(start, end)
        >>> print elapsed
        1 day, 03:10:04h

        :param start: start time, in seconds.
        :param end: end time, in seconds.
        """

        in_day = end - start
        days = 0

        if in_day > 86400.0:
            days = int(elapsed / 86400.0)
            in_day = end - start - 86400 * days

        rediv = lambda ll, b: list(divmod(ll[0], b)) + ll[1:]
        elapsed = "%02d:%02d:%02d.%02d" % tuple(reduce(
            rediv, [[in_day * 1000, ],
                    1000, 60, 60]))

        start_index = 0
        unit = 'h'

        if elapsed[start_index:start_index + 2] == "00":
            start_index += 3
            unit = 'm'

        if elapsed[start_index:start_index + 2] == "00":
            start_index += 3
            unit = 's'

        if days > 0:
            return "%d days, %s%c" % (days, elapsed[start_index:], unit)
        else:
            return "%s%c" % (elapsed[start_index:], unit)
