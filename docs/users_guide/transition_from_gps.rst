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

GPS Python package renamed to GS
---------------------------------

One major change regarding custom plugins is that the *GPS* Python package has
been renamed to *GS*. However, we have introduced a fallback mechanism that
allows your plugins to continue referencing *GPS* while transitioning.

GPS.Entity removal
------------------

The :class:`GPS.Entity` is now obsolete and has been removed. If your custom
plugins were making use of this class, you should now use use the libadalang
Python API (available in the GNAT Studio Python interpreter) instead to get
information about entities present in your source code.

More information about libadalang and its Python API can be found
`here <http://docs.adacore.com/live/wave/libadalang/html/libadalang_ug/>`_.

Here is an example that shows how to retrieve the enclosing subprogram
of a given location.

.. code-block:: python

   import libadalang as lal


   def current_subprogram(self):
      # Return the LAL node corresponding to the subprogram enclosing the
      # current context, or None
      curloc = self.location()
      buf = GPS.EditorBuffer.get(curloc.file(), open=False)
      if not buf:
         return False
      unit = buf.get_analysis_unit()
      node = unit.root.lookup(lal.Sloc(curloc.line(), curloc.column()))
      return get_enclosing_subprogram(node)
