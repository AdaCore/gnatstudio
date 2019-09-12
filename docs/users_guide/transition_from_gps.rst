How to transition from GPS to GNAT Studio
==========================================

This section describes how to transition from GPS to GNAT Studio.

User Settings
^^^^^^^^^^^^^

No action is required regarding user settings: the GPS user settings stored
under :file:`$HOME/.gps` will be automatically copied to a new
:file:`$HOME/.gnatstudio` directory when launching GNAT Studio for the first
time. So all you settings should be correctly preserved after installing
GNAT Studio.

Environment Variables
^^^^^^^^^^^^^^^^^^^^^

The old :file:`GPS_HOME`, :file:`GPS_DOC_PATH` and :file:`GPS_CUSTOM_PATH`
environment variables have been respectively renamed to :file:`GNATSTUDIO_HOME`,
:file:`GNATSTUDIO_DOC_PATH` and :file:`GNATSTUDIO_CUSTOM_PATH`.

Note that GNAT Studio fallbacks to the old variables when no values are given
for the new ones.

Custom Scripts and Plugins
^^^^^^^^^^^^^^^^^^^^^^^^^^

Change of executable name and custom scripts
----------------------------------------------

Custom scripts/wrappers that run :file:`gps` or :file:`gps_cli` need
to been adapted to respectively run :file:`gnatstudio` and
:file:`gnatstudio_cli` instead.


GPS Python package renamed to GS
---------------------------------

One major change regarding custom plugins is that the *GPS* Python package has
been renamed to *GS*. However, we have introduced a fallback mechanism that
allows your plugins to still reference *GPS* while transitioning.

GPS.Entity removal
------------------

The :class:`GPS.Entity` is now obsolescent.

You should now use use the libadalang Python API (available directly from GPS
plugins) instead to get information about entities present in your source code.

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
