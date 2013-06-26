Cross-references
----------------

New cross-reference engine :feature:`(GPS -- 2013-01-24 -- L530-025)`
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

GPS and GNATbench both have a new xref engine, based on sqlite. This new
engine is expected to bring extra performance (since the xref database
becomes persistent across sessions and GPS will no longer need to parse ALI
files on the fly), as well as more stability (by using an external process
to fill the database, and relying on sqlite's very good stability).
