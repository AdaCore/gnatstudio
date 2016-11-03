from . import core
import os


@vcs2.register_vcs
class CVS(core.VCS):

    @staticmethod
    def discover_repo(file):
        return core.find_admin_directory(file, 'CVS')
