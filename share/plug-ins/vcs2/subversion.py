from . import core
import os


@core.register_vcs
class Subversion(core.VCS):

    @staticmethod
    def discover_repo(file):
        return core.find_admin_directory(file, '.svn')
