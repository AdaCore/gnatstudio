import vcs2
import os


@vcs2.register_vcs
class Subversion(vcs2.VCS):

    @staticmethod
    def find_repo(file):
        return vcs2.find_admin_directory(file, '.svn')
