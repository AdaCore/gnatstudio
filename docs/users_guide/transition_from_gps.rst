How to transition from GPS to GNAT Studio
==========================================

GPS has been renamed to GNAT Studio. If you were a GPS user, this section
describes any adaptation needed your end to handle this transition.

User Settings
^^^^^^^^^^^^^

The GPS settings were stored in directory in the :file:`.gps` directory in
your home directory (:file:`%USERPROFILE` on Windows; :file:`$HOME` on Linux).

The GNAT Studio settings are stored in the :file:`.gnatstudio` directory
instead.

The first time GNAT Studio is launched, the GPS settings will be automatically
copied over to the GNAT Studio directory.

Environment Variables
^^^^^^^^^^^^^^^^^^^^^

GPS was reading the environment varibles :file:`GPS_HOME`, :file:`GPS_DOC_PATH`
and :file:`GPS_CUSTOM_PATH`. These have been respectively renamed to
:file:`GNATSTUDIO_HOME`, :file:`GNATSTUDIO_DOC_PATH` and
:file:`GNATSTUDIO_CUSTOM_PATH`.

As a convenience, GNAT Studio falls back to reading the :file:`GPS_*`
variables if the :file:`GNATSTUDIO_*` ones are not defined.

Change of executable name
^^^^^^^^^^^^^^^^^^^^^^^^^

The executables :file:`gps[.exe]` and :file:`gps_cli[.exe]` are now called
:file:`gnatstudio[.exe]` and :file:`gnatstudio_cli[.exe]`.

If you have any scripts or links referring them, the scripts will need
to be adjusted.

If this is convenient to you, you can create a link called :file:`gps`
pointing to :file:`gnatstudio`.

Custom Scripts and Plugins
^^^^^^^^^^^^^^^^^^^^^^^^^^

GPS.Entity removal
------------------

The :class:`GPS.Entity` API (and the associated Python plugin layer) has been
removed entirely in this branch. Customization now happens through Ada modules
and XML configuration only; there is no embedded Python interpreter or hook
surface.
