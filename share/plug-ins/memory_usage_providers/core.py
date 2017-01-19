"""
Base plugin to implement support for memory usage providers.
"""

import GPS


class MemoryUsageProvider(GPS.MemoryUsageProvider):
    """
    To create a new memory usage provider, extend this class, and then call:

        @core.register_memory_usage_provider
        class MyMemoryUsageProvider(core.MemoryUsageProvider):
            pass

    """

    #######################
    # Overridable methods #
    #######################

    def __init__(self, *args, **kwargs):
        """
        Called just after creating the instance.
        See `register_memory_usage_provider` for more information.
        """

        pass

    def async_fetch_memory_regions(self, visitor):
        """
        Fetch the memory regions of the last built executable.

        Depending on the way to retrieve this data, this function may be
        asynchronous: call `visitor.on_memory_regions_fetched` method
        to notify that the operation ended.

        :param GPS.MemoryUsageProviderVisitor: the object used to report
        when the operation has finished.
        """

        pass


class register_memory_usage_provider:
    """
    A decorator used to register a new memory usage provider.

    :param str name: the name of the memory usage provider
    :param args: passed to the class constructor
    :param kwargs: pass to the class constructor
    """

    def __init__(self, name, *args, **kwargs):
        self.name = name
        self.args = args
        self.kwargs = kwargs

    def __call__(self, klass):
        GPS.MemoryUsageProvider._register(
            self.name or klass.__name__,
            construct=lambda: klass(*self.args, **self.kwargs))
        return klass
