"""
This plugin should not be activated. It is provided only for
backward compatibility with older versions of GPS (up to 6.0.2).

To activate the Emacs key shortcuts, please go to the
    /Edit/Key Shortcuts
dialog instead.
"""

import GPS

msg = """The emacs.py plugin is now obsolete, and has been disabled.
Please use /Edit/Key Shortcuts instead.
Your setup has been transitioned, but you need to restart GPS.

"""

GPS.Console().write(msg)

# Enable the Emacs key shortcuts
GPS.History.add("key-theme", "emacs")

# Disable the emacs.py plugin

f = "%sstartup.xml" % (GPS.get_home_dir(), )
old = file(f).read()
new = old.replace('"emacs.py"', '"obsolete-emacs.py"')
file(f, "w").write(new)
