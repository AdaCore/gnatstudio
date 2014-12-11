"""
This plug-in should not be activated. It is provided only for
backward compatibility with older versions of GPS (up to 6.0.2).

To activate the Emacs key shortcuts, please go to the
    /Edit/Key Shortcuts
dialog instead.
"""

msg = """The emacs.py plug-in is now obsolete, and has been disabled.
Please use /Edit/Key Shortcuts instead.
Your setup has been transitioned, but you need to restart GPS.

"""

import GPS
GPS.Console().write(msg)

# Enable the Emacs key shortcuts
GPS.History.add("key-theme", "emacs")

# Disable the emacs.py plug-in

f = "%sstartup.xml" % (GPS.get_home_dir(), )
old = file(f).read()
new = old.replace('"emacs.py"', '"obsolete-emacs.py"')
file(f, "w").write(new)
