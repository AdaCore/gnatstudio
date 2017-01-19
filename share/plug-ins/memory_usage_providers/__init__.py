import GPS

if hasattr(GPS, "MemoryUsageProvider"):
    from . import core
    from . import ld
