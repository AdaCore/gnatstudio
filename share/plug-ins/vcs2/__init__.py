import GPS

if hasattr(GPS, "VCS2"):
    from . import core
    from . import git
    from . import gerrit
    from . import cvs
    from . import subversion
    from . import clearcase
