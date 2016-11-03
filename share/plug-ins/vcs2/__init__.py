"""
Base type to implement support for new VCS engines in GPS
"""

import GPS

if hasattr(GPS, "VCS2"):
    from . import core
    from . import git
    from . import cvs
    from . import subversion
