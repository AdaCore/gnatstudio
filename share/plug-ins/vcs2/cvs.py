import vcs2
import os


@vcs2.register_vcs
class CVS(vcs2.VCS):

    @staticmethod
    def discover_repo(file):
        return vcs2.find_admin_directory(file, 'CVS')
