"""git support plug-in
"""


###########################################################################
## No user customization below this line
###########################################################################

import os.path

def from_git_root (filename):
    "Given a filename it returns the pathname relative to the Git root"
    dir=os.getcwd()
    full=""
    while not os.path.exists(dir + '/.git'):
        full = os.path.basename (dir) + "/" + full
        dir = os.path.dirname (dir)
    full = full + filename
    return full
