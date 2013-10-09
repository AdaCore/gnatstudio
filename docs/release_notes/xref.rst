Cross-references
----------------

New cross-reference engine
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. NF-60-L530-025 New cross-reference engine (2013-01-24)

GPS and GNATbench both have a new xref engine, based on sqlite. This new
engine is expected to bring extra performance (since the xref database
becomes persistent across sessions and GPS will no longer need to parse ALI
files on the fly), as well as more stability (by using an external process
to fill the database, and relying on sqlite's very good stability).

Find all references
~~~~~~~~~~~~~~~~~~~

.. NF-60-M919-026 GPS: Find All References displays context (2013-09-19)

When you perform a "Find All References", each reference will also display
the context of that reference (in general, the subprogram within which the
reference is found).
